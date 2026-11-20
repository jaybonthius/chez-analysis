;; Follows the datum procedures of s/read.ss so diagnostics arrive in the
;; order Chez raises them.
(define (parse-text t lx initial-mode)
  (define s (text-string t))
  (define n (string-length s))
  (define pos 0)
  (define pending #f)
  (define tokens '())
  (define mode initial-mode)
  (define transitions (list (cons 0 initial-mode)))
  (define forms '())
  (define unclosed-count 0)
  (define reported-closer #f)
  ;; Graph marks are scoped to one top-level datum and the atmosphere before
  ;; it, as in Chez's reader, which keeps one table per call.
  (define marks (make-eqv-hashtable))
  (define references (make-eqv-hashtable))

  (define (diagnostic-count) (lexer-diagnostic-count lx))

  (define (error! start end fmt . args)
    (add-diagnostic! lx start end (apply format fmt args)))

  (define (token-error! tok fmt . args)
    (apply error! (token-start tok) (token-end tok) fmt args))

  (define (apply-directive! name end)
    (let ([syntax-name (reader-mode-syntax mode)] [fold (reader-mode-fold mode)])
      (case name
        [(r6rs) (lexer-r6rs?-set! lx #t) (set! syntax-name 'r6rs)]
        [(chezscheme) (lexer-r6rs?-set! lx #f) (set! syntax-name 'chezscheme)]
        [(fold-case) (lexer-fold?-set! lx #t) (set! fold 'fold)]
        [(no-fold-case) (lexer-fold?-set! lx #f) (set! fold 'no-fold)])
      (set! mode (canonical-reader-mode syntax-name fold))
      (set! transitions (cons (cons end mode) transitions))))

  (define (next!)
    (cond
      [pending (let ([tok pending]) (set! pending #f) tok)]
      [(fx= pos n) #f]
      [else
       (lexer-diagnosed?-set! lx #f)
       (lexer-unterminated?-set! lx #f)
       (let-values ([(kind subkind end) (scan-token lx pos)])
         (let ([tok (make-token kind subkind pos end
                      (if (lexer-unterminated? lx) unterminated-flags no-flags))])
           (when (eq? kind 'directive) (apply-directive! subkind end))
           (set! pos end)
           (set! tokens (cons tok tokens))
           tok))]))

  (define (trivia? tok)
    (memq (token-kind tok) '(whitespace line-comment block-comment directive script-header)))

  (define (datum-comment-prefix? tok)
    (and (eq? (token-kind tok) 'prefix) (eq? (token-subkind tok) 'datum-comment)))

  (define (significant! children)
    (let ([tok (next!)])
      (cond
        [(not tok) #f]
        [(trivia? tok) (add! children tok) (significant! children)]
        [(datum-comment-prefix? tok) (add! children (parse-datum-comment tok)) (significant! children)]
        [else tok])))

  (define (make-children first) (box (list first)))
  (define (add! children x) (set-box! children (cons x (unbox children))))

  (define (finish kind children start errors-before)
    (let ([items (unbox children)])
      (make-node kind start (datum-end (car items)) (frozen-vector (reverse items))
        (fx> (diagnostic-count) errors-before))))

  (define (closer-message tok)
    (if (eq? (token-subkind tok) 'paren) "unexpected close parenthesis" "unexpected close bracket"))

  (define (parse-datum tok)
    (case (token-kind tok)
      [(open) (parse-list tok)]
      [(prefix) (parse-abbreviation tok)]
      [(close)
       (token-error! tok (closer-message tok))
       (set! reported-closer tok)
       (set! pending tok)
       #f]
      [(dot) (token-error! tok "unexpected dot (.)") tok]
      [(error)
       (when (eq? (token-subkind tok) 'fasl)
         (token-error! tok "unsupported old fasl format detected---use new format with binary i/o"))
       tok]
      [(reference) (note-reference! tok) tok]
      [else tok]))

  (define (graph-number tok)
    (string->number (substring s (fx+ (token-start tok) 1) (fx- (token-end tok) 1))))

  (define (note-reference! tok)
    (let ([k (graph-number tok)])
      (unless (hashtable-contains? references k)
        (hashtable-set! references k tok))))

  (define (abbreviation-name subkind)
    (case subkind
      [(box) "box"]
      [(mark) "graph mark"]
      [else (symbol->string subkind)]))

  (define (parse-abbreviation tok)
    (let ([children (make-children tok)] [before (diagnostic-count)]
          [subkind (token-subkind tok)])
      (let ([k (and (eq? subkind 'mark) (graph-number tok))])
        (when k
          (when (hashtable-contains? marks k)
            (token-error! tok "duplicate mark #~s= seen" k))
          (hashtable-set! marks k #f))
        (let ([next (significant! children)])
          (if next
              (let ([datum (parse-datum next)])
                (when datum
                  (add! children datum)
                  (when k (hashtable-set! marks k datum))))
              (error! (token-start tok) n "unexpected end-of-file reading ~a" (abbreviation-name subkind)))))
      (finish 'abbreviation children (token-start tok) before)))

  (define (parse-datum-comment tok)
    (let ([children (make-children tok)] [before (diagnostic-count)])
      (let ([next (significant! children)])
        (if next
            (let ([datum (parse-datum next)])
              (when datum (add! children datum)))
            (error! (token-start tok) n "unexpected end-of-file reading ~a" "s-expression comment")))
      (finish 'datum-comment children (token-start tok) before)))

  (define (list-name subkind)
    (case subkind
      [(paren) "list"]
      [(bracket) "bracketed list"]
      [(vector sized-vector) "vector"]
      [(fxvector sized-fxvector) "fxvector"]
      [(flvector sized-flvector) "flvector"]
      [(bytevector sized-bytevector) "bytevector"]
      [(stencil-vector sized-stencil-vector) "stencil vector"]
      [(record) "record"]))

  (define (element-kind subkind)
    (case subkind
      [(fxvector sized-fxvector) 'fixnum]
      [(flvector sized-flvector) 'flonum]
      [(bytevector sized-bytevector) 'octet]
      [else #f]))

  (define (atomic? tok)
    (memq (token-kind tok) '(symbol number string character boolean gensym primitive special)))

  (define (number-value tok)
    (and (eq? (token-kind tok) 'number) (number-token-value s tok (lexer-r6rs? lx))))

  ;; rd-fxvector, rd-flvector and rd-bytevector-check take each element's
  ;; token as it comes, closers and dots included.
  (define (check-element! kind tok)
    (unless (and (eq? (token-kind tok) 'error) (not (eq? (token-subkind tok) 'fasl)))
      (let ([v (and (atomic? tok) (number-value tok))])
        (case kind
          [(fixnum) (unless (fixnum? v) (token-error! tok "non-fixnum found in fxvector"))]
          [(flonum) (unless (flonum? v) (token-error! tok "non-flonum found in flvector"))]
          [(octet)
           (unless (and (fixnum? v) (fx<= 0 v 255))
             (if (atomic? tok)
                 (token-error! tok "invalid value ~a found in bytevector" (element-text tok v))
                 (token-error! tok "non-octet found in bytevector")))]))))

  (define (element-text tok v)
    (cond
      [(number? v) (format "~s" v)]
      [(memq (token-kind tok) '(symbol gensym))
       (let-values ([(name slashed?) (decode-token-name s (token-start tok) (lexer-r6rs? lx) (lexer-fold? lx))]) name)]
      [else (substring s (token-start tok) (token-end tok))]))

  (define (record-name? tok)
    (memq (token-kind tok) '(symbol gensym)))

  ;; Chez checks a sized opener's size or mask before reading the elements.
  (define (opener-size tok subkind)
    (case subkind
      [(sized-vector sized-fxvector sized-flvector sized-bytevector sized-stencil-vector)
       (let* ([start (fx+ (token-start tok) 1)]
              [digits-end (let loop ([i start])
                            (if (char<=? #\0 (string-ref s i) #\9) (loop (fx+ i 1)) i))]
              [size (string->number (substring s start digits-end))])
         (if (eq? subkind 'sized-stencil-vector)
             (if (and (fixnum? size) (fx< size (fxsll 1 (stencil-vector-mask-width))))
                 (fxpopcount size)
                 (begin
                   (error! start (fx- (token-end tok) 1) "invalid stencil vector mask ~s" size)
                   #f))
             (if (fixnum? size)
                 size
                 (begin
                   (error! start (fx- (token-end tok) 1) "invalid ~a length ~s" (list-name subkind) size)
                   #f))))]
      [(stencil-vector) (token-error! tok "mask required for stencil vector") #f]
      [else #f]))

  (define (parse-list tok)
    (let* ([subkind (token-subkind tok)]
           [start (token-start tok)]
           [children (make-children tok)]
           [before (diagnostic-count)]
           [closer (if (memq subkind '(bracket record)) 'bracket 'paren)]
           [dotted? (memq subkind '(paren bracket))]
           [element (element-kind subkind)]
           [size (opener-size tok subkind)])
      (define (close! closer-tok)
        (add! children closer-tok)
        (finish 'list children start before))
      (let loop ([state 'items] [count 0])
        (let ([next (significant! children)])
          (cond
            [(not next)
             (error! start n "unexpected end-of-file reading ~a" (list-name subkind))
             (set! unclosed-count (fx+ unclosed-count 1))
             (finish 'list children start before)]
            [(and (eq? subkind 'record) (fx= count 0) (not (record-name? next)))
             (token-error! next "non-symbol found after #[")
             (case (token-kind next)
               [(close) (close! next)]
               [(dot) (add! children next) (loop state (fx+ count 1))]
               [else (let ([datum (parse-datum next)])
                       (when datum (add! children datum))
                       (loop state (fx+ count 1)))])]
            [(eq? (token-kind next) 'close)
             (cond
               [(eq? (token-subkind next) closer)
                (when (and dotted? (eq? state 'after-dot))
                  (token-error! next "expected one item after dot (.)"))
                (when (and (eq? subkind 'sized-stencil-vector) size (fx< count size))
                  (token-error! next "not enough stencil vector elements supplied"))]
               [element (check-element! element next)]
               [(and dotted? (not (eq? state 'after-dot)))
                (error! start (token-end next)
                  (if (eq? subkind 'paren)
                      "parenthesized list terminated by bracket"
                      "bracketed list terminated by parenthesis"))]
               [else (token-error! next (closer-message next))])
             (close! next)]
            [(eq? (token-kind next) 'dot)
             (cond
               [element (check-element! element next)]
               [(and dotted? (eq? state 'after-tail)) (token-error! next "unexpected dot")]
               [(and dotted? (eq? state 'items) (fx> count 0)) (void)]
               [else (token-error! next "unexpected dot (.)")])
             (add! children next)
             (loop (if dotted? (if (eq? state 'after-tail) state 'after-dot) state) count)]
            [else
             (when (and dotted? (eq? state 'after-tail))
               (token-error! next "more than one item found after dot (.)"))
             (when element
               (check-element! element next)
               (when (and size (fx>= count size))
                 (error! start (token-end next) "too many ~a elements supplied" (list-name subkind))))
             (let ([datum (parse-datum next)])
               (when datum
                 (add! children datum)
                 (when (and size (not element) (fx>= count size))
                   (error! start (datum-end datum)
                     "too many ~a elements supplied" (list-name subkind)))))
             (loop (case state [(after-dot after-tail) 'after-tail] [else state]) (fx+ count 1))])))))

  ;; Chez's reader resolves graph references after reading a top-level
  ;; datum, walking it depth first and entering a mark's datum at the first
  ;; reference or mark it meets; a missing mark is reported at the first
  ;; reference that named it, wherever the walk found it.
  (define (check-graph! datum)
    (unless (fx= (hashtable-size references) 0)
      (let ([visited (make-eqv-hashtable)])
        (define (visit-mark! k)
          (unless (hashtable-contains? visited k)
            (hashtable-set! visited k #t)
            (if (hashtable-contains? marks k)
                (let ([d (hashtable-ref marks k #f)]) (when d (walk d)))
                (token-error! (hashtable-ref references k #f) "mark #~s= missing" k))))
        (define (walk x)
          (if (token? x)
              (when (eq? (token-kind x) 'reference) (visit-mark! (graph-number x)))
              (case (node-kind x)
                [(abbreviation)
                 (let ([prefix (vector-ref (node-children x) 0)])
                   (if (eq? (token-subkind prefix) 'mark)
                       (visit-mark! (graph-number prefix))
                       (vector-for-each walk (node-data x))))]
                [(list)
                 (let ([subkind (token-subkind (vector-ref (node-children x) 0))])
                   (unless (element-kind subkind)
                     (let ([data (node-data x)])
                       (do ([i (if (eq? subkind 'record) 1 0) (fx+ i 1)])
                           ((fx>= i (vector-length data)))
                         (walk (vector-ref data i))))))]
                [else (void)])))
        (walk datum))))

  (define (parse-root)
    (let ([children (box '())])
      (let loop ()
        (hashtable-clear! marks)
        (hashtable-clear! references)
        (let ([next (significant! children)])
          (cond
            [(not next) (void)]
            [(eq? (token-kind next) 'close)
             (unless (eq? next reported-closer)
               (token-error! next (closer-message next)))
             (let ([unmatched (make-token 'close (token-subkind next) (token-start next) (token-end next)
                                unmatched-flags)])
               (set! tokens (cons unmatched (cdr tokens)))
               (add! children unmatched))
             (loop)]
            [(and (eq? (token-kind next) 'special) (eq? (token-subkind next) 'eof))
             (add! children next)
             (when (fx< pos n)
               (let ([tail (make-token 'unread-tail #f pos n no-flags)])
                 (set! tokens (cons tail tokens))
                 (set! pos n)
                 (add! children tail)))]
            [(eq? (token-kind next) 'dot)
             (token-error! next "unexpected dot (.)")
             (add! children next)
             (loop)]
            [else
             (let* ([form-mode mode] [datum (parse-datum next)])
               (add! children datum)
               (set! forms (cons (make-top-level-form datum form-mode) forms))
               (check-graph! datum))
             (loop)])))
      (make-node 'root 0 n (frozen-vector (reverse (unbox children)))
        (fx> (diagnostic-count) 0))))

  (let ([root (parse-root)])
    (make-cst t root
      (frozen-vector (reverse tokens))
      (frozen-vector
        (append (reverse (lexer-diagnostics lx))
                (if (fx> unclosed-count 0) (missing-closer-hints t root unclosed-count) '())))
      (frozen-vector (reverse transitions))
      (frozen-vector (reverse forms)))))

;; A CST is shared between threads, so nothing in it may be mutable.
(define (frozen-vector items) (vector->immutable-vector (list->vector items)))

;; A number Chez would take unbounded time to compute is never a fixnum,
;; flonum, octet or plausible library version, so big stands in for it.
(define (number-token-value s tok r6rs?)
  (let-values ([(outcome value) (classify-number s (token-start tok) (token-end tok) r6rs?)])
    (and (eq? outcome 'number)
         (case value
           [(zero) 0]
           [(big) 'big]
           [else (string->number (substring s (token-start tok) (token-end tok)) 10)]))))

;; Where an opener is never closed, Chez reports the end of the input, and an
;; unclosed list runs to the end, so the closer is missing inside the last
;; top-level datum. It is probably missing before the first line whose
;; leading datum sits at or left of its list's opener, the list having
;; swallowed its own siblings. A list at column 0 can wrap a whole file with
;; an unindented body, as (begin and (let () do in Chez's source, so there
;; such a line counts only after one indented past the opener. Failing that,
;; the closer is probably missing before the first line whose leading datum
;; is indented less than its list's previous line-leading datum.
(define (missing-closer-hints t root count)
  (let* ([s (text-string t)] [n (string-length s)] [dedents '()] [outdents '()]
         [line-count (text-line-count t)]
         [starts (make-fxvector line-count)]
         [leads (make-fxvector line-count)]
         [indents (make-fxvector line-count)])
    (define (line-of offset)
      (let loop ([lo 0] [hi line-count])
        (if (fx= (fx+ lo 1) hi)
            lo
            (let ([mid (fxsrl (fx+ lo hi) 1)])
              (if (fx< offset (fxvector-ref starts mid)) (loop lo mid) (loop mid hi))))))
    (define (leads-line? offset)
      (fx= offset (fxvector-ref leads (line-of offset))))
    (define (column offset)
      (let ([line (line-of offset)])
        (fx+ (fxvector-ref indents line) (fx- offset (fxvector-ref leads line)))))
    (define (walk x)
      (when (node? x)
        (let ([data (node-data x)])
          (when (eq? (node-kind x) 'list)
            (let ([opener (column (node-start x))])
              (let loop ([i 0] [previous #f] [indented? #f])
                (when (fx< i (vector-length data))
                  (let* ([child (vector-ref data i)] [start (datum-start child)])
                    (if (leads-line? start)
                        (let ([col (column start)])
                          (when (and previous (fx< col previous))
                            (set! dedents (cons child dedents)))
                          (if (and (or indented? (fx> opener 0)) (fx<= col opener))
                              (set! outdents (cons child outdents))
                              (loop (fx+ i 1) col (or indented? (and (not previous) (fx> col opener))))))
                        (loop (fx+ i 1) previous indented?)))))))
          (vector-for-each walk data))))
    (define (by-start xs) (sort (lambda (a b) (fx< (datum-start a) (datum-start b))) xs))
    ;; Tabs count to the next multiple of eight in the indentation only, so a
    ;; column past the indentation needs no scan of its line.
    (do ([line 0 (fx+ line 1)]) ((fx= line line-count))
      (let ([start (text-line-start t line)] [end (text-line-end t line)])
        (fxvector-set! starts line start)
        (let loop ([i start] [col 0])
          (cond
            [(and (fx< i end) (char=? (string-ref s i) #\space)) (loop (fx+ i 1) (fx+ col 1))]
            [(and (fx< i end) (char=? (string-ref s i) #\tab)) (loop (fx+ i 1) (fx* 8 (fx+ (fxquotient col 8) 1)))]
            [else (fxvector-set! leads line i) (fxvector-set! indents line col)]))))
    (vector-for-each
      (lambda (x) (when (and (node? x) (fx= (node-end x) n)) (walk x)))
      (node-data root))
    (let loop ([cs (append (by-start outdents) (by-start dedents))] [k count] [lines '()] [hints '()])
      (if (or (null? cs) (fx= k 0))
          (reverse hints)
          (let* ([c (car cs)] [line (line-of (datum-start c))])
            (if (memv line lines)
                (loop (cdr cs) k lines hints)
                (loop (cdr cs) (fx- k 1) (cons line lines)
                  (cons (make-diagnostic 'missing-closer (severity information) (datum-start c) (datum-end c)
                          "a closing parenthesis or bracket is probably missing before this line"
                          '() no-tags)
                        hints))))))))
