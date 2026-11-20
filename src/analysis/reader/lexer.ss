;; Follows s/read.ss's token states one for one, so token boundaries and the
;; first error agree with Chez's reader.
;;
;; A span's end is where Chez's reader stood when it raised: after the
;; character it read, before a character it only peeked at.

(define-record-type lexer
  (nongenerative) (sealed #t)
  (fields string length
          (mutable r6rs?) (mutable fold?)
          ;; Diagnostics, newest first, and whether the current token has one:
          ;; Chez stops at a token's first error, so later ones in the same
          ;; token would only restate it.
          (mutable diagnostics) (mutable diagnostic-count) (mutable diagnosed?)
          (mutable unterminated?)
          (mutable out) (mutable slashed?) quiet?))

(define (new-lexer s r6rs? fold?)
  (make-lexer s (string-length s) r6rs? fold? '() 0 #f #f #f #f #f))

(define (new-decoding-lexer s r6rs? fold? out)
  (make-lexer s (string-length s) r6rs? fold? '() 0 #f #f out #f #t))

(define (lex-error! lx start end fmt . args)
  (unless (lexer-diagnosed? lx)
    (lexer-diagnosed?-set! lx #t)
    (unless (lexer-quiet? lx) (add-diagnostic! lx start end (apply format fmt args)))))

;; Chez can read one character past the end before it raises; offsets past
;; the text name no character, so the span stops at the end.
(define (add-diagnostic! lx start end message)
  (let ([n (lexer-length lx)])
    (lexer-diagnostic-count-set! lx (fx+ (lexer-diagnostic-count lx) 1))
    (lexer-diagnostics-set! lx
      (cons (make-diagnostic 'read-error (severity error) (fxmin start n) (fxmin end n) message '() no-tags)
            (lexer-diagnostics lx)))))

(define no-tags (diagnostic-tag-set))

(define (nonstandard! lx start end what)
  (when (lexer-r6rs? lx)
    (lex-error! lx start end "~a syntax is not allowed in #!r6rs mode" what)))

(define (nonstandard-delimiter! lx start end c)
  (when (lexer-r6rs? lx)
    (lex-error! lx start end "delimiter ~a is not allowed in #!r6rs mode" c)))

(define (emit! lx c)
  (let ([out (lexer-out lx)]) (when out (write-char c out))))

(define (emit-string! lx str)
  (let ([out (lexer-out lx)]) (when out (put-string out str))))

(define class-other 0)
(define class-symbol 1)
(define class-delimiter 2)
(define class-chez-delimiter 3)

(define ascii-classes
  (let ([v (make-bytevector 128 class-other)])
    (for-each (lambda (c) (bytevector-u8-set! v (char->integer c) class-symbol))
      (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-?*!=><$%&/:^_~+.@"))
    (for-each (lambda (c) (bytevector-u8-set! v (char->integer c) class-delimiter))
      (string->list " ()[]\";#\t\n\v\f\r"))
    (for-each (lambda (c) (bytevector-u8-set! v (char->integer c) class-chez-delimiter))
      (string->list "{}'`,"))
    v))

(define (char-class c)
  (let ([i (char->integer c)])
    (if (fx< i 128)
        (bytevector-u8-ref ascii-classes i)
        (if (char-whitespace? c) class-delimiter class-other))))

(define (delimiter? c) (fx= (char-class c) class-delimiter))
(define (chez-delimiter? c) (fx= (char-class c) class-chez-delimiter))

;; Chez's $constituent? and $subsequent? for characters above ASCII, which
;; read.ss handles by explicit cases.
(define (constituent? c)
  (and (fx> (char->integer c) 127)
       (memq (char-general-category c) '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co))
       #t))

(define (subsequent? c)
  (and (memq (char-general-category c) '(Nd Mc Me)) #t))

(define (hex-digit-value c)
  (cond
    [(char<=? #\0 c #\9) (fx- (char->integer c) 48)]
    [(char<=? #\a c #\f) (fx- (char->integer c) 87)]
    [(char<=? #\A c #\F) (fx- (char->integer c) 55)]
    [else #f]))

(define (ascii-letter? c) (or (char<=? #\a c #\z) (char<=? #\A c #\Z)))
(define (octal-digit? c) (char<=? #\0 c #\7))
(define (scalar-value? v) (and (fixnum? v) (or (fx<= v #xD7FF) (fx<= #xE000 v #x10FFFF))))

;; Hex escapes may be arbitrarily long; any value past the last code point
;; is equally invalid.
(define (add-hex-digit v d) (if (> v #x10FFFF) v (+ (* v 16) d)))

;; rd-token-to-delimiter.
(define (to-delimiter lx p)
  (let ([s (lexer-string lx)] [n (lexer-length lx)])
    (let loop ([p p])
      (if (or (fx= p n)
              (let ([k (char-class (string-ref s p))])
                (or (fx= k class-delimiter) (fx= k class-chez-delimiter))))
          p
          (loop (fx+ p 1))))))

(define (valid-prefix? x names)
  (exists (lambda (y)
            (and (fx<= (string-length x) (string-length y))
                 (string=? x (substring y 0 (string-length x)))))
          names))

;; rd-token-symbol.
(define (scan-symbol lx bfp p)
  (let ([s (lexer-string lx)] [n (lexer-length lx)])
    (let loop ([p p])
      (if (fx= p n)
          p
          (let* ([c (string-ref s p)] [k (char-class c)])
            (cond
              [(fx= k class-symbol) (emit! lx c) (loop (fx+ p 1))]
              [(fx= k class-delimiter) p]
              [(char=? c #\\) (scan-symbol-escape lx bfp p loop)]
              [(char=? c #\|)
               (nonstandard! lx bfp (fx+ p 1) "|...| symbol escape")
               (let bar ([q (fx+ p 1)])
                 (cond
                   [(fx= q n)
                    (lex-error! lx bfp n "unexpected end-of-file reading ~a" "symbol")
                    (lexer-unterminated?-set! lx #t)
                    n]
                   [(char=? (string-ref s q) #\|)
                    (lexer-slashed?-set! lx #t)
                    (loop (fx+ q 1))]
                   [else (emit! lx (string-ref s q)) (bar (fx+ q 1))]))]
              [(fx= k class-chez-delimiter) (nonstandard-delimiter! lx bfp (fx+ p 1) c) p]
              [(or (constituent? c) (subsequent? c)) (emit! lx c) (loop (fx+ p 1))]
              [else
               (nonstandard! lx bfp (fx+ p 1) (format "character ~c in symbol" c))
               (emit! lx c)
               (loop (fx+ p 1))]))))))

(define (scan-symbol-escape lx bfp p continue)
  (let ([s (lexer-string lx)] [n (lexer-length lx)])
    (cond
      [(fx= (fx+ p 1) n)
       (lex-error! lx bfp n "unexpected end-of-file reading ~a" "symbol")
       n]
      [(char=? (string-ref s (fx+ p 1)) #\x)
       (let hex ([q (fx+ p 2)] [v 0])
         (if (fx= q n)
             (begin (lex-error! lx bfp n "unexpected end-of-file reading ~a" "symbol") n)
             (let ([c (string-ref s q)])
               (cond
                 [(hex-digit-value c) => (lambda (d) (hex (fx+ q 1) (add-hex-digit v d)))]
                 [(char=? c #\;)
                  (if (scalar-value? v)
                      (emit! lx (integer->char v))
                      (lex-error! lx p (fx+ q 1) "invalid code point value ~s in symbol hex escape"
                        (hex-escape-value s (fx+ p 2) q)))
                  (continue (fx+ q 1))]
                 [else
                  (lex-error! lx p q "invalid character ~c in symbol hex escape" c)
                  (continue q)]))))]
      [else
       (nonstandard! lx bfp (fx+ p 2) "non-hex back-slash symbol escape")
       (emit! lx (string-ref s (fx+ p 1)))
       (lexer-slashed?-set! lx #t)
       (continue (fx+ p 2))])))

;; The exact value an over-long hex escape names, for the message only.
(define (hex-escape-value s start end)
  (string->number (substring s start end) 16))

;; rd-token-intern-nonstandard.
(define (nonstandard-symbol! lx start end)
  (when (and (lexer-r6rs? lx) (not (lexer-diagnosed? lx)) (not (lexer-quiet? lx)))
    (let-values ([(name slashed?) (decode-token-name (lexer-string lx) start (lexer-r6rs? lx) (lexer-fold? lx))])
      (nonstandard! lx start end (format "~a symbol" name)))))

;; rd-token-number-or-symbol.
(define (scan-number-or-symbol lx bfp p)
  (let ([s (lexer-string lx)] [n (lexer-length lx)] [r6rs? (lexer-r6rs? lx)])
    (define (finish end)
      (let-values ([(outcome value) (classify-number s bfp end r6rs?)])
        (case outcome
          [(number) (values 'number end)]
          [(norep) (lex-error! lx bfp end "cannot represent ~a" (substring s bfp end)) (values 'number end)]
          [(!r6rs) (nonstandard! lx bfp end (format "~a number" (substring s bfp end))) (values 'number end)]
          [else (nonstandard! lx bfp end (format "~a symbol" (substring s bfp end))) (values 'symbol end)])))
    (let loop ([p p])
      (if (fx= p n)
          (finish p)
          (let ([c (string-ref s p)])
            (cond
              [(or (char<=? #\0 c #\9) (char<=? #\a c #\z) (char<=? #\A c #\Z)
                   (memv c '(#\- #\+ #\. #\/ #\@ #\|)))
               (emit! lx c)
               (loop (fx+ p 1))]
              [(char=? c #\#) (if r6rs? (finish p) (begin (emit! lx c) (loop (fx+ p 1))))]
              [(delimiter? c) (finish p)]
              [(chez-delimiter? c) (nonstandard-delimiter! lx bfp (fx+ p 1) c) (finish p)]
              [else
               (let ([end (scan-symbol lx bfp p)])
                 (nonstandard-symbol! lx bfp end)
                 (values 'symbol end))]))))))

;; rd-token-number: after a # prefix the token can only be a number.
(define (scan-number lx bfp p)
  (let ([s (lexer-string lx)] [n (lexer-length lx)])
    (let loop ([p p])
      (let ([end (cond
                   [(fx= p n) p]
                   [(let ([c (string-ref s p)])
                      (and (not (char=? c #\#))
                           (or (delimiter? c)
                               (and (chez-delimiter? c)
                                    (begin (nonstandard-delimiter! lx bfp (fx+ p 1) c) #t)))))
                    p]
                   [else #f])])
        (if end
            (let-values ([(outcome value) (classify-number s bfp end (lexer-r6rs? lx))])
              (case outcome
                [(number) (values 'number end)]
                [(#f bogus)
                 (if (and (not outcome) (fx= end n))
                     (lex-error! lx bfp end "unexpected end-of-file reading ~a" "number")
                     (lex-error! lx bfp end "invalid number syntax ~a" (substring s bfp end)))
                 (values 'error end)]
                [(!r6rs)
                 (nonstandard! lx bfp end (format "~a number" (substring s bfp end)))
                 (values 'number end)]
                [else (lex-error! lx bfp end "cannot represent ~a" (substring s bfp end)) (values 'number end)]))
            (loop (fx+ p 1)))))))

;; rd-token-delimiter.
(define (delimited lx bfp p kind what)
  (let ([s (lexer-string lx)] [n (lexer-length lx)])
    (if (fx= p n)
        (values kind p)
        (let ([c (string-ref s p)])
          (cond
            [(delimiter? c) (values kind p)]
            [(chez-delimiter? c) (nonstandard-delimiter! lx bfp p c) (values kind p)]
            [else
             (lex-error! lx bfp p "invalid delimiter ~a for ~a" c what)
             (values 'error (to-delimiter lx p))])))))

(define (scan-boolean lx bfp p value)
  (let ([s (lexer-string lx)] [n (lexer-length lx)])
    (if (and (fx< p n) (char-alphabetic? (string-ref s p)))
        (let ([word (if value "true" "false")])
          (let loop ([i 1])
            (let ([q (fx+ bfp i 1)])
              (cond
                [(fx= q n)
                 (lex-error! lx bfp n "unexpected end-of-file reading ~a" "boolean")
                 (values 'error n)]
                [(not (char-ci=? (string-ref s q) (string-ref word i)))
                 (lex-error! lx bfp q "invalid boolean #~a~c" (substring word 0 i)
                   (char-downcase (string-ref s q)))
                 (values 'error (to-delimiter lx q))]
                [(fx= i (fx- (string-length word) 1))
                 (nonstandard! lx bfp (fx+ q 1) "alternative boolean")
                 (delimited lx bfp (fx+ q 1) 'boolean "boolean")]
                [else (loop (fx+ i 1))]))))
        (delimited lx bfp p 'boolean "boolean"))))

(define r6rs-character-names
  '(("nul" . #\nul) ("alarm" . #\alarm) ("backspace" . #\backspace) ("tab" . #\tab)
    ("linefeed" . #\newline) ("newline" . #\newline) ("vtab" . #\vtab) ("page" . #\page)
    ("return" . #\return) ("esc" . #\esc) ("space" . #\space) ("delete" . #\delete)))

;; The names in Chez's char-name table when no program has added any.
(define chez-character-names
  '(("space" . #\space) ("tab" . #\tab) ("return" . #\return) ("page" . #\page)
    ("linefeed" . #\linefeed) ("newline" . #\newline) ("backspace" . #\backspace)
    ("rubout" . #\rubout) ("nul" . #\nul) ("bel" . #\bel) ("vt" . #\vt) ("esc" . #\esc)
    ("vtab" . #\vtab) ("delete" . #\rubout) ("alarm" . #\bel) ("nel" . #\nel) ("ls" . #\ls)))

(define (character-names lx)
  (if (lexer-r6rs? lx) r6rs-character-names chez-character-names))

;; rd-token-charname.
(define (scan-character-name lx bfp q)
  (let* ([s (lexer-string lx)] [n (lexer-length lx)]
         [d (to-delimiter lx q)]
         [name (substring s q d)]
         [key (if (lexer-fold? lx) (string-foldcase name) name)])
    (cond
      [(assoc key (character-names lx)) (values 'character d)]
      [(and (fx= d n) (valid-prefix? name (map car (character-names lx))))
       (lex-error! lx bfp n "unexpected end-of-file reading ~a" "character")
       (values 'error n)]
      [else
       (lex-error! lx bfp d "invalid character name #\\~a" name)
       (values 'error d)])))

;; rd-token-char.
(define (scan-character lx bfp q)
  (let ([s (lexer-string lx)] [n (lexer-length lx)])
    (define (peek i) (and (fx< i n) (string-ref s i)))
    (if (fx= q n)
        (begin (lex-error! lx bfp n "unexpected end-of-file reading ~a" "character") (values 'error n))
        (let ([c (string-ref s q)] [c1 (peek (fx+ q 1))])
          (cond
            [(char=? c #\x)
             (cond
               [(not c1) (values 'character (fx+ q 1))]
               [(hex-digit-value c1) (scan-character-hex lx bfp q)]
               [else (delimited lx bfp (fx+ q 1) 'character "character")])]
            [(ascii-letter? c)
             (cond
               [(not c1) (values 'character (fx+ q 1))]
               [(ascii-letter? c1) (scan-character-name lx bfp q)]
               [else (delimited lx bfp (fx+ q 1) 'character "character")])]
            [(octal-digit? c)
             (cond
               [(and c1 (octal-digit? c1))
                (nonstandard! lx bfp (fx+ q 1) "octal character")
                (let ([c2 (peek (fx+ q 2))])
                  (cond
                    [(not c2)
                     (lex-error! lx bfp n "unexpected end-of-file reading ~a" "character")
                     (values 'error n)]
                    [(octal-digit? c2)
                     (if (fx> (fx+ (fx* 64 (fx- (char->integer c) 48))
                                   (fx* 8 (fx- (char->integer c1) 48))
                                   (fx- (char->integer c2) 48))
                              255)
                         (begin
                           (lex-error! lx bfp (fx+ q 3) "invalid character #\\~a~a~a" c c1 c2)
                           (values 'error (to-delimiter lx (fx+ q 3))))
                         (delimited lx bfp (fx+ q 3) 'character "character"))]
                    [else
                     (lex-error! lx bfp (fx+ q 3) "invalid character #\\~a~a" c c1)
                     (values 'error (to-delimiter lx (fx+ q 2)))]))]
               [else (delimited lx bfp (fx+ q 1) 'character "character")])]
            [else (delimited lx bfp (fx+ q 1) 'character "character")])))))

;; rd-token-char-hex.
(define (scan-character-hex lx bfp q)
  (let ([s (lexer-string lx)] [n (lexer-length lx)])
    (define (finish end)
      (let ([v (let loop ([i (fx+ q 1)] [v 0])
                 (if (fx= i end) v (loop (fx+ i 1) (add-hex-digit v (hex-digit-value (string-ref s i))))))])
        (if (scalar-value? v)
            (values 'character end)
            (begin
              (lex-error! lx bfp end "invalid hex character escape ~a" (substring s q end))
              (values 'error end)))))
    (let loop ([r (fx+ q 1)])
      (if (fx= r n)
          (finish r)
          (let ([c (string-ref s r)])
            (cond
              [(hex-digit-value c) (loop (fx+ r 1))]
              [(delimiter? c) (finish r)]
              [(chez-delimiter? c) (nonstandard-delimiter! lx bfp (fx+ r 1) c) (finish r)]
              [else (scan-character-name lx bfp q)]))))))

(define (scan-string lx bfp)
  (let ([s (lexer-string lx)] [n (lexer-length lx)])
    (define (eof!)
      (lex-error! lx bfp n "unexpected end-of-file reading ~a" "string")
      (lexer-unterminated?-set! lx #t)
      n)
    (define (intraline? c) (or (char=? c #\tab) (eq? (char-general-category c) 'Zs)))
    (define (line-end? c) (memv c '(#\newline #\nel #\ls)))
    ;; rd-token-string-skipwhite
    (define (skip-white p)
      (cond
        [(fx= p n) (eof!)]
        [(intraline? (string-ref s p)) (skip-white (fx+ p 1))]
        [else (loop p)]))
    ;; rd-token-string-whitespace.
    (define (after-backslash-white p)
      (let ([c (string-ref s p)])
        (cond
          [(line-end? c) (skip-white (fx+ p 1))]
          [(char=? c #\return)
           (cond
             [(fx= (fx+ p 1) n) (eof!)]
             [(memv (string-ref s (fx+ p 1)) '(#\newline #\nel)) (skip-white (fx+ p 2))]
             [else (skip-white (fx+ p 1))])]
          [(intraline? c) (if (fx= (fx+ p 1) n) (eof!) (after-backslash-white (fx+ p 1)))]
          [else
           (lex-error! lx bfp (fx+ p 1) "unexpected character ~c after \\<intraline whitespace> in string" c)
           (loop p)])))
    (define (escape p)
      (if (fx= (fx+ p 1) n)
          (eof!)
          (let ([c (string-ref s (fx+ p 1))])
            (cond
              [(assv c '((#\\ . #\\) (#\" . #\") (#\n . #\newline) (#\a . #\alarm) (#\b . #\backspace)
                         (#\f . #\page) (#\r . #\return) (#\t . #\tab) (#\v . #\vtab)))
               => (lambda (escaped) (emit! lx (cdr escaped)) (loop (fx+ p 2)))]
              [(char=? c #\x)
               (let hex ([q (fx+ p 2)] [v 0])
                 (if (fx= q n)
                     (eof!)
                     (let ([c1 (string-ref s q)])
                       (cond
                         [(hex-digit-value c1) => (lambda (d) (hex (fx+ q 1) (add-hex-digit v d)))]
                         [(char=? c1 #\;)
                          (if (scalar-value? v)
                              (emit! lx (integer->char v))
                              (lex-error! lx p (fx+ q 1) "invalid code point value ~s in string hex escape"
                                (hex-escape-value s (fx+ p 2) q)))
                          (loop (fx+ q 1))]
                         [else
                          (lex-error! lx p q "invalid character ~c in string hex escape" c1)
                          (loop q)]))))]
              [(or (line-end? c) (char=? c #\return) (intraline? c)) (after-backslash-white (fx+ p 1))]
              [(octal-digit? c)
               (nonstandard! lx bfp (fx+ p 2) "octal string-character")
               (cond
                 [(fx= (fx+ p 2) n) (eof!)]
                 [(not (octal-digit? (string-ref s (fx+ p 2))))
                  (lex-error! lx p (fx+ p 2) "invalid string character \\~c" c)
                  (loop (fx+ p 2))]
                 [(fx= (fx+ p 3) n) (eof!)]
                 [(not (octal-digit? (string-ref s (fx+ p 3))))
                  (lex-error! lx p (fx+ p 3) "invalid string character \\~c~c" c (string-ref s (fx+ p 2)))
                  (loop (fx+ p 3))]
                 [else
                  (let* ([c1 (string-ref s (fx+ p 2))] [c2 (string-ref s (fx+ p 3))]
                         [v (fx+ (fx* 64 (fx- (char->integer c) 48)) (fx* 8 (fx- (char->integer c1) 48))
                                 (fx- (char->integer c2) 48))])
                    (if (fx> v 255)
                        (lex-error! lx p (fx+ p 4) "invalid string character \\~c~c~c" c c1 c2)
                        (emit! lx (integer->char v)))
                    (loop (fx+ p 4)))])]
              [(char=? c #\')
               (nonstandard! lx bfp (fx+ p 2) "\\' string character")
               (emit! lx c)
               (loop (fx+ p 2))]
              [else
               (lex-error! lx p (fx+ p 2) "invalid string character \\~c" c)
               (loop (fx+ p 2))]))))
    (define (loop p)
      (if (fx= p n)
          (eof!)
          (let ([c (string-ref s p)])
            (cond
              [(char=? c #\") (fx+ p 1)]
              [(char=? c #\\) (escape p)]
              [(char=? c #\return)
               (cond
                 [(fx= (fx+ p 1) n) (eof!)]
                 [(memv (string-ref s (fx+ p 1)) '(#\newline #\nel)) (emit! lx #\newline) (loop (fx+ p 2))]
                 [else (emit! lx #\newline) (loop (fx+ p 1))])]
              [(line-end? c) (emit! lx #\newline) (loop (fx+ p 1))]
              [else (emit! lx c) (loop (fx+ p 1))]))))
    (loop (fx+ bfp 1))))

(define (scan-block-comment lx bfp)
  (let ([s (lexer-string lx)] [n (lexer-length lx)])
    (define (eof!)
      (lex-error! lx bfp n "unexpected end-of-file reading ~a" "block comment")
      (lexer-unterminated?-set! lx #t)
      n)
    (let loop ([p (fx+ bfp 2)] [depth 0])
      (if (fx= p n)
          (eof!)
          (let ([c (string-ref s p)])
            (cond
              [(char=? c #\|)
               (cond
                 [(fx= (fx+ p 1) n) (eof!)]
                 [(char=? (string-ref s (fx+ p 1)) #\#)
                  (if (fx= depth 0) (fx+ p 2) (loop (fx+ p 2) (fx- depth 1)))]
                 [else (loop (fx+ p 1) depth)])]
              [(char=? c #\#)
               (cond
                 [(fx= (fx+ p 1) n) (eof!)]
                 [(char=? (string-ref s (fx+ p 1)) #\|) (loop (fx+ p 2) (fx+ depth 1))]
                 [else (loop (fx+ p 1) depth)])]
              [else (loop (fx+ p 1) depth)]))))))

(define (scan-gensym lx bfp)
  (let* ([s (lexer-string lx)] [n (lexer-length lx)] [q (fx+ bfp 2)])
    (define (eof!)
      (lex-error! lx bfp n "unexpected end-of-file reading ~a" "gensym")
      (lexer-unterminated?-set! lx #t)
      (values 'gensym n))
    (if (fx= q n)
        (eof!)
        (let ([e1 (scan-symbol lx bfp q)])
          (let white ([r e1])
            (cond
              [(fx= r n) (eof!)]
              [(memv (string-ref s r) '(#\space #\newline #\tab)) (white (fx+ r 1))]
              [else
               ;; The unique name is not part of the decoded name, and its
               ;; escapes do not decide whether the pretty name folds.
               (let ([e2 (let ([out (lexer-out lx)] [slashed? (lexer-slashed? lx)])
                           (lexer-out-set! lx #f)
                           (let ([e (scan-symbol lx bfp r)])
                             (lexer-out-set! lx out)
                             (lexer-slashed?-set! lx slashed?)
                             e))])
                 (cond
                   [(fx= e2 n) (eof!)]
                   [(char=? (string-ref s e2) #\}) (values 'gensym (fx+ e2 1))]
                   [else
                    (lex-error! lx bfp e2 "expected close brace terminating gensym syntax")
                    (values 'error e2)]))]))))))


;; rd-token-hash-bang: the four directives need no delimiter, so #!r6rsfoo
;; is the directive followed by foo.
(define directive-names '("r6rs" "fold-case" "no-fold-case" "chezscheme"))

(define (scan-hash-bang lx bfp)
  (let* ([s (lexer-string lx)] [n (lexer-length lx)] [q (fx+ bfp 2)])
    (let loop ([i 0] [candidates directive-names])
      (cond
        [(find (lambda (name) (fx= (string-length name) i)) candidates)
         => (lambda (name) (values 'directive (string->symbol name) (fx+ q i)))]
        [(fx= (fx+ q i) n)
         (lex-error! lx bfp n "unexpected end-of-file reading ~a" "#! syntax")
         (values 'error #f n)]
        [else
         (let* ([c (string-ref s (fx+ q i))]
                [matching (filter (lambda (name) (char=? (string-ref name i) c)) candidates)])
           (if (null? matching)
               (let* ([d (to-delimiter lx (fx+ q i))] [name (substring s q d)])
                 (cond
                   [(member name '("eof" "bwp" "base-rtd"))
                    (nonstandard! lx bfp d (string-append "#!" name))
                    (values 'special (string->symbol name) d)]
                   [(and (fx= d n) (valid-prefix? name '("eof" "bwp" "base-rtd")))
                    (lex-error! lx bfp n "unexpected end-of-file reading ~a" "#! syntax")
                    (values 'error #f n)]
                   [else
                    (lex-error! lx bfp d "invalid syntax #!~a" name)
                    (values 'error #f d)]))
               (loop (fx+ i 1) matching)))]))))

(define vector-prefixes '("fx" "fl" "u8" "s"))

;; rd-token-hash-v and rd-token-hash-num-v.
(define (scan-hash-v lx bfp q digits)
  (let* ([s (lexer-string lx)] [n (lexer-length lx)]
         [d (to-delimiter lx q)]
         [name (substring s q d)]
         [prefix (if digits (string-append "#" digits "v") "#v")]
         [message-prefixes (if digits '("fx" "u8") vector-prefixes)])
    (cond
      [(fx= d n)
       (if (valid-prefix? name message-prefixes)
           (lex-error! lx bfp n "unexpected end-of-file reading ~a" "#v prefix")
           (lex-error! lx bfp n "invalid syntax ~a~a" prefix name))
       (values 'error #f n)]
      [(char=? (string-ref s d) #\()
       (let ([subkind (cond
                        [(string=? name "fx") (if digits 'sized-fxvector 'fxvector)]
                        [(string=? name "fl") (if digits 'sized-flvector 'flvector)]
                        [(string=? name "u8") (if digits 'sized-bytevector 'bytevector)]
                        [(string=? name "s") (if digits 'sized-stencil-vector 'stencil-vector)]
                        [else #f])])
         (cond
           [(not subkind)
            (lex-error! lx bfp (fx+ d 1) "invalid syntax ~a~a(" prefix name)
            (values 'error #f d)]
           [else
            (case subkind
              [(fxvector) (nonstandard! lx bfp (fx+ d 1) "#vfx(...) fxvector")]
              [(flvector) (nonstandard! lx bfp (fx+ d 1) "#vfl(...) flvector")]
              [(stencil-vector) (nonstandard! lx bfp (fx+ d 1) "#vs(...) stencil vector")]
              [(sized-fxvector) (nonstandard! lx bfp (fx+ d 1) "#<n>vfx(...) fxvector")]
              [(sized-flvector) (nonstandard! lx bfp (fx+ d 1) "#<n>vfl(...) flvector")]
              [(sized-bytevector) (nonstandard! lx bfp (fx+ d 1) "#<n>vu8(...) bytevector")]
              [(sized-stencil-vector) (nonstandard! lx bfp (fx+ d 1) "#<n>vs(...) stencil vector")]
              [else (void)])
            (values 'open subkind (fx+ d 1))]))]
      [else
       (if (valid-prefix? name message-prefixes)
           (lex-error! lx bfp (fx+ d 1) "expected left paren after ~a~a prefix" prefix name)
           (lex-error! lx bfp (fx+ d 1) "invalid syntax ~a~a~a" prefix name (string-ref s d)))
       (values 'error #f d)])))

;; rd-token-hash-num.
(define (scan-hash-number lx bfp)
  (let ([s (lexer-string lx)] [n (lexer-length lx)])
    (let loop ([r (fx+ bfp 1)])
      (if (and (fx< r n) (char<=? #\0 (string-ref s r) #\9))
          (loop (fx+ r 1))
          (let ([digits (substring s (fx+ bfp 1) r)])
            (if (fx= r n)
                (begin
                  (lex-error! lx bfp n "unexpected end-of-file reading ~a" "# prefix")
                  (values 'error #f n))
                (let ([c (string-ref s r)])
                  (cond
                    [(char=? c #\()
                     (nonstandard! lx bfp (fx+ r 1) "#<n>(...) vector")
                     (values 'open 'sized-vector (fx+ r 1))]
                    [(char-ci=? c #\r)
                     (nonstandard! lx bfp (fx+ r 1) "#<n>r number prefix")
                     (let-values ([(kind end) (scan-number lx bfp (fx+ r 1))])
                       (values kind #f end))]
                    [(char-ci=? c #\q)
                     (lex-error! lx bfp (fx+ r 1) "outdated object file format")
                     (values 'error #f (to-delimiter lx (fx+ r 1)))]
                    [(char=? c #\#)
                     (nonstandard! lx bfp (fx+ r 1) "#<n># insert")
                     (values 'reference #f (fx+ r 1))]
                    [(char=? c #\=)
                     (nonstandard! lx bfp (fx+ r 1) "#<n>= mark")
                     (values 'prefix 'mark (fx+ r 1))]
                    [(char=? c #\v)
                     (scan-hash-v lx bfp (fx+ r 1) digits)]
                    [(char=? c #\%)
                     (let ([level (string->number digits)])
                       (cond
                         [(memv level '(2 3))
                          (nonstandard! lx bfp (fx+ r 1) "#<n>% primitive")
                          (values 'primitive (if (eqv? level 2) 'level-2 'level-3) (scan-symbol lx bfp (fx+ r 1)))]
                         [else
                          (lex-error! lx bfp (fx+ r 1) "invalid sharp-sign prefix ~a~a" (substring s bfp r) c)
                          (values 'error #f (to-delimiter lx (fx+ r 1)))]))]
                    [else
                     (lex-error! lx bfp (fx+ r 1) "invalid sharp-sign prefix ~a~a" (substring s bfp r) c)
                     (values 'error #f (to-delimiter lx r))]))))))))

;; rd-token-hash.
(define (scan-hash lx bfp)
  (let ([s (lexer-string lx)] [n (lexer-length lx)] [p (fx+ bfp 1)])
    (if (fx= p n)
        (begin
          (lex-error! lx bfp n "unexpected end-of-file reading ~a" "# prefix")
          (values 'error #f n))
        (let ([c (string-ref s p)])
          (case c
            [(#\f #\F) (let-values ([(kind end) (scan-boolean lx bfp (fx+ p 1) #f)]) (values kind #f end))]
            [(#\t #\T) (let-values ([(kind end) (scan-boolean lx bfp (fx+ p 1) #t)]) (values kind #f end))]
            [(#\\) (let-values ([(kind end) (scan-character lx bfp (fx+ p 1))]) (values kind #f end))]
            [(#\() (values 'open 'vector (fx+ p 1))]
            [(#\') (values 'prefix 'syntax (fx+ p 1))]
            [(#\`) (values 'prefix 'quasisyntax (fx+ p 1))]
            [(#\,) (if (and (fx< (fx+ p 1) n) (char=? (string-ref s (fx+ p 1)) #\@))
                       (values 'prefix 'unsyntax-splicing (fx+ p 2))
                       (values 'prefix 'unsyntax (fx+ p 1)))]
            [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (scan-hash-number lx bfp)]
            [(#\@) (values 'error 'fasl (fx+ p 1))]
            [(#\[) (nonstandard! lx bfp (fx+ p 1) "#[...] record") (values 'open 'record (fx+ p 1))]
            [(#\{) (nonstandard! lx bfp (fx+ p 1) "#{...} gensym")
                   (let-values ([(kind end) (scan-gensym lx bfp)]) (values kind #f end))]
            [(#\&) (nonstandard! lx bfp (fx+ p 1) "#& box") (values 'prefix 'box (fx+ p 1))]
            [(#\;) (values 'prefix 'datum-comment (fx+ p 1))]
            [(#\!) (if (and (fx= bfp 0) (fx< (fx+ p 1) n) (memv (string-ref s (fx+ p 1)) '(#\space #\/)))
                       (values 'script-header #f
                         (let loop ([q (fx+ p 1)])
                           (cond
                             [(fx= q n) n]
                             [(char=? (string-ref s q) #\newline) (fx+ q 1)]
                             [else (loop (fx+ q 1))])))
                       (scan-hash-bang lx bfp))]
            [(#\x #\X #\o #\O #\b #\B #\d #\D #\i #\I #\e #\E)
             (let-values ([(kind end) (scan-number lx bfp (fx+ p 1))]) (values kind #f end))]
            [(#\v) (scan-hash-v lx bfp (fx+ p 1) #f)]
            [(#\%) (nonstandard! lx bfp (fx+ p 1) "#% primitive")
                   (values 'primitive #f (scan-symbol lx bfp (fx+ p 1)))]
            [(#\:) (nonstandard! lx bfp (fx+ p 1) "#: gensym")
                   (values 'gensym #f (scan-symbol lx bfp (fx+ p 1)))]
            [(#\|) (values 'block-comment #f (scan-block-comment lx bfp))]
            [else
             (lex-error! lx bfp (fx+ p 1) "invalid sharp-sign prefix #~c" c)
             (values 'error #f (to-delimiter lx p))])))))

;; rd-token-dot, rd-token-dot-dot and rd-token-dot-dot-dot.
(define (scan-dot lx bfp)
  (let* ([s (lexer-string lx)] [n (lexer-length lx)] [p (fx+ bfp 1)])
    (cond
      [(fx= p n) (values 'dot p)]
      [(char=? (string-ref s p) #\.)
       (let ([q (fx+ p 1)])
         (cond
           ;; Chez reads past the end here, so its span ends one beyond the text.
           [(fx= q n) (nonstandard! lx bfp (fx+ n 1) ".. symbol") (emit-string! lx "..") (values 'symbol q)]
           [(char=? (string-ref s q) #\.)
            (let ([r (fx+ q 1)])
              (cond
                [(or (fx= r n) (delimiter? (string-ref s r))) (emit-string! lx "...") (values 'symbol r)]
                [(chez-delimiter? (string-ref s r))
                 (nonstandard-delimiter! lx bfp (fx+ r 1) (string-ref s r))
                 (emit-string! lx "...")
                 (values 'symbol r)]
                [else
                 (emit-string! lx "...")
                 (let ([end (scan-symbol lx bfp r)])
                   (nonstandard-symbol! lx bfp end)
                   (values 'symbol end))]))]
           [else
            (emit-string! lx "..")
            (let ([end (scan-symbol lx bfp q)])
              (nonstandard-symbol! lx bfp end)
              (values 'symbol end))]))]
      [(delimiter? (string-ref s p)) (values 'dot p)]
      [(chez-delimiter? (string-ref s p))
       (nonstandard-delimiter! lx bfp p (string-ref s p))
       (values 'dot p)]
      [else (emit! lx #\.) (scan-number-or-symbol lx bfp p)])))

;; rd-token-minus and rd-token-plus.
(define (scan-sign lx bfp c)
  (let* ([s (lexer-string lx)] [n (lexer-length lx)] [p (fx+ bfp 1)])
    (cond
      [(or (fx= p n) (delimiter? (string-ref s p))) (emit! lx c) (values 'symbol p)]
      [(and (char=? c #\-) (char=? (string-ref s p) #\>))
       (emit! lx c)
       (values 'symbol (scan-symbol lx bfp p))]
      [(chez-delimiter? (string-ref s p))
       (nonstandard-delimiter! lx bfp p (string-ref s p))
       (emit! lx c)
       (values 'symbol p)]
      [else (emit! lx c) (scan-number-or-symbol lx bfp p)])))

(define (line-comment-end s n p)
  (if (or (fx= p n) (memv (string-ref s p) '(#\newline #\return #\nel #\ls)))
      p
      (line-comment-end s n (fx+ p 1))))

(define (whitespace-end s n p)
  (if (and (fx< p n) (char-whitespace? (string-ref s p)))
      (whitespace-end s n (fx+ p 1))
      p))

;; rd-token.
(define (scan-token lx p)
  (let* ([s (lexer-string lx)] [n (lexer-length lx)] [c (string-ref s p)])
    (cond
      [(ascii-letter? c)
       (emit! lx c)
       (values 'symbol #f (scan-symbol lx p (fx+ p 1)))]
      [(char-whitespace? c) (values 'whitespace #f (whitespace-end s n (fx+ p 1)))]
      [else
       (case c
         [(#\() (values 'open 'paren (fx+ p 1))]
         [(#\)) (values 'close 'paren (fx+ p 1))]
         [(#\[) (values 'open 'bracket (fx+ p 1))]
         [(#\]) (values 'close 'bracket (fx+ p 1))]
         [(#\') (values 'prefix 'quote (fx+ p 1))]
         [(#\`) (values 'prefix 'quasiquote (fx+ p 1))]
         [(#\,) (if (and (fx< (fx+ p 1) n) (char=? (string-ref s (fx+ p 1)) #\@))
                    (values 'prefix 'unquote-splicing (fx+ p 2))
                    (values 'prefix 'unquote (fx+ p 1)))]
         [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
          (emit! lx c)
          (let-values ([(kind end) (scan-number-or-symbol lx p (fx+ p 1))]) (values kind #f end))]
         [(#\- #\+) (let-values ([(kind end) (scan-sign lx p c)]) (values kind #f end))]
         [(#\.) (let-values ([(kind end) (scan-dot lx p)]) (values kind #f end))]
         [(#\;) (values 'line-comment #f (line-comment-end s n (fx+ p 1)))]
         [(#\#) (scan-hash lx p)]
         [(#\") (values 'string #f (scan-string lx p))]
         [(#\* #\= #\< #\> #\/ #\! #\$ #\% #\& #\: #\? #\^ #\_ #\~)
          (emit! lx c)
          (values 'symbol #f (scan-symbol lx p (fx+ p 1)))]
         [(#\| #\\) (values 'symbol #f (scan-symbol lx p p))]
         [(#\{) (nonstandard! lx p (fx+ p 1) "open brace") (emit! lx c) (values 'symbol #f (fx+ p 1))]
         [(#\}) (nonstandard! lx p (fx+ p 1) "close brace") (emit! lx c) (values 'symbol #f (fx+ p 1))]
         [else
          (if (constituent? c)
              (begin (emit! lx c) (values 'symbol #f (scan-symbol lx p (fx+ p 1))))
              (let ([end (scan-symbol lx p p)])
                (nonstandard-symbol! lx p end)
                (values 'symbol #f end)))])])))

(define (decode-string-token s start r6rs?)
  (let-values ([(out get) (open-string-output-port)])
    (let ([lx (new-decoding-lexer s r6rs? #f out)])
      (scan-string lx start)
      (and (not (lexer-diagnosed? lx)) (get)))))

;; A name is what Chez's reader collects in its token buffer while scanning
;; the token, so decoding scans it again with the buffer kept.
(define (decode-token-name s start r6rs? fold?)
  (let-values ([(out get) (open-string-output-port)])
    (let ([lx (new-decoding-lexer s r6rs? fold? out)])
      (scan-token lx start)
      (values (get) (lexer-slashed? lx)))))
