(library (analysis text)
  (export
    ;; texts
    make-text text? text-string text-length text-line-count
    text-line-start text-line-end text-offset-line text-substring text-hash
    decode-source-bytes bytevector-hash
    ;; LSP positions
    position-encoding position-encoding-set
    text-position->offset text-position->offset/strict text-offset->position
    ;; edits
    make-text-edit text-edit? text-edit-start text-edit-end text-edit-replacement
    text-apply-edit apply-edits text-difference-edit
    ;; offset maps
    identity-offset-map offset-map? offset-map-add-edit offset-map-compose
    offset-map-offset offset-map-span offset-map-span-back line-ending-offset-map
    edge-bias edge-bias-set)
  (import (chezscheme))

  (include "analysis/include/text-identity.ss")

  (define-enumeration position-encoding (utf-16 utf-32) position-encoding-set)
  (define-enumeration edge-bias (left right) edge-bias-set)

  (define (line-starts-of s)
    (let ([n (string-length s)])
      (let loop ([i 0] [starts '(0)] [count 1])
        (if (fx= i n)
            (let ([v (make-fxvector count)])
              (let fill ([starts starts] [j (fx- count 1)])
                (unless (null? starts)
                  (fxvector-set! v j (car starts))
                  (fill (cdr starts) (fx- j 1))))
              v)
            (let ([c (string-ref s i)])
              (cond
                [(char=? c #\newline) (loop (fx+ i 1) (cons (fx+ i 1) starts) (fx+ count 1))]
                [(char=? c #\return)
                 (let ([next (if (and (fx< (fx+ i 1) n) (char=? (string-ref s (fx+ i 1)) #\newline))
                                 (fx+ i 2)
                                 (fx+ i 1))])
                   (loop next (cons next starts) (fx+ count 1)))]
                [else (loop (fx+ i 1) starts count)]))))))

  (define-record-type text
    (nongenerative) (sealed #t)
    (fields string line-starts (mutable hash-cache))
    (protocol
      (lambda (new)
        (lambda (s)
          (unless (string? s) (assertion-violationf 'make-text "~s is not a string" s))
          (let ([s (string->immutable-string s)])
            (new s (line-starts-of s) #f))))))

  (define (check-text who t)
    (unless (text? t) (assertion-violationf who "~s is not a text" t)))

  (define (check-offset who t offset)
    (let ([n (string-length (text-string t))])
      (unless (and (fixnum? offset) (fx<= 0 offset n))
        (assertion-violationf who "~s is not an offset in a text of length ~s" offset n))))

  (define (check-line who t line)
    (let ([count (fxvector-length (text-line-starts t))])
      (unless (and (fixnum? line) (fx<= 0 line) (fx< line count))
        (assertion-violationf who "~s is not a line of a text with ~s lines" line count))))

  (define (text-length t)
    (check-text 'text-length t)
    (string-length (text-string t)))

  (define (text-line-count t)
    (check-text 'text-line-count t)
    (fxvector-length (text-line-starts t)))

  (define (text-line-start t line)
    (check-text 'text-line-start t)
    (check-line 'text-line-start t line)
    (fxvector-ref (text-line-starts t) line))

  (define (line-end t line)
    (let ([starts (text-line-starts t)] [s (text-string t)])
      (if (fx= line (fx- (fxvector-length starts) 1))
          (string-length s)
          (let ([next (fxvector-ref starts (fx+ line 1))])
            (if (and (fx>= next 2)
                     (char=? (string-ref s (fx- next 1)) #\newline)
                     (char=? (string-ref s (fx- next 2)) #\return)
                     (fx>= (fx- next 2) (fxvector-ref starts line)))
                (fx- next 2)
                (fx- next 1))))))

  (define (text-line-end t line)
    (check-text 'text-line-end t)
    (check-line 'text-line-end t line)
    (line-end t line))

  (define (offset-line t offset)
    (let ([starts (text-line-starts t)])
      (let loop ([lo 0] [hi (fxvector-length starts)])
        (if (fx= (fx+ lo 1) hi)
            lo
            (let ([mid (fxsrl (fx+ lo hi) 1)])
              (if (fx< offset (fxvector-ref starts mid))
                  (loop lo mid)
                  (loop mid hi)))))))

  (define (text-offset-line t offset)
    (check-text 'text-offset-line t)
    (check-offset 'text-offset-line t offset)
    (offset-line t offset))

  (define (text-substring t start end)
    (check-text 'text-substring t)
    (check-offset 'text-substring t start)
    (check-offset 'text-substring t end)
    (unless (fx<= start end)
      (assertion-violationf 'text-substring "start ~s is after end ~s" start end))
    (substring (text-string t) start end))

  ;; Any thread may fill the cache; each writes the same value, so a race is
  ;; harmless.
  (define (text-hash t)
    (check-text 'text-hash t)
    (or (text-hash-cache t)
        (let ([h (source-text-hash (text-string t))])
          (text-hash-cache-set! t h)
          h)))

  (define (check-encoding who enc)
    (unless (enum-set-member? enc (enum-set-universe (position-encoding-set)))
      (assertion-violationf who "~s is not a position encoding" enc)))

  (define (check-position who line character)
    (unless (and (integer? line) (exact? line) (>= line 0))
      (assertion-violationf who "~s is not a line number" line))
    (unless (and (integer? character) (exact? character) (>= character 0))
      (assertion-violationf who "~s is not a character number" character)))

  (define (line-offset t enc start end character)
    (if (eq? enc 'utf-32)
        (values (if (< character (fx- end start)) (+ start character) end) #f)
        (let ([s (text-string t)])
          (let loop ([i start] [units 0])
            (cond
              [(= units character) (values i #f)]
              [(fx= i end) (values end #f)]
              [else
               (let ([width (if (fx>= (char->integer (string-ref s i)) #x10000) 2 1)])
                 (if (> (+ units width) character)
                     (values i #t)
                     (loop (fx+ i 1) (fx+ units width))))])))))

  (define (text-position->offset t enc line character)
    (check-text 'text-position->offset t)
    (check-encoding 'text-position->offset enc)
    (check-position 'text-position->offset line character)
    (if (>= line (fxvector-length (text-line-starts t)))
        (string-length (text-string t))
        (let-values ([(offset inside-pair?)
                      (line-offset t enc (fxvector-ref (text-line-starts t) line) (line-end t line) character)])
          offset)))

  (define (text-position->offset/strict t enc line character)
    (check-text 'text-position->offset/strict t)
    (check-encoding 'text-position->offset/strict enc)
    (check-position 'text-position->offset/strict line character)
    (let ([count (fxvector-length (text-line-starts t))])
      (cond
        [(> line count) #f]
        [(= line count) (and (= character 0) (string-length (text-string t)))]
        [else
         (let ([start (fxvector-ref (text-line-starts t) line)] [end (line-end t line)])
           (let-values ([(offset inside-pair?) (line-offset t enc start end character)])
             (and (not inside-pair?)
                  (or (fx< offset end)
                      (= character (code-units t enc start end)))
                  offset)))])))

  (define (code-units t enc start end)
    (if (eq? enc 'utf-32)
        (fx- end start)
        (let ([s (text-string t)])
          (let loop ([i start] [units 0])
            (if (fx= i end)
                units
                (loop (fx+ i 1)
                      (fx+ units (if (fx>= (char->integer (string-ref s i)) #x10000) 2 1))))))))

  (define (text-offset->position t enc offset)
    (check-text 'text-offset->position t)
    (check-encoding 'text-offset->position enc)
    (check-offset 'text-offset->position t offset)
    (let* ([line (offset-line t offset)]
           [start (fxvector-ref (text-line-starts t) line)])
      (values line (code-units t enc start offset))))

  (define-record-type text-edit
    (nongenerative) (sealed #t)
    (fields start end replacement)
    (protocol
      (lambda (new)
        (lambda (start end replacement)
          (unless (and (fixnum? start) (fx>= start 0))
            (assertion-violationf 'make-text-edit "~s is not an offset" start))
          (unless (and (fixnum? end) (fx>= end start))
            (assertion-violationf 'make-text-edit "~s is not an offset at or after ~s" end start))
          (unless (string? replacement)
            (assertion-violationf 'make-text-edit "~s is not a string" replacement))
          (new start end (string->immutable-string replacement))))))

  (define (text-apply-edit t edit)
    (check-text 'text-apply-edit t)
    (unless (text-edit? edit) (assertion-violationf 'text-apply-edit "~s is not a text edit" edit))
    (let ([s (text-string t)] [start (text-edit-start edit)] [end (text-edit-end edit)])
      (unless (fx<= end (string-length s))
        (assertion-violationf 'text-apply-edit "edit end ~s is past the text's length ~s" end (string-length s)))
      (make-text
        (string-append (substring s 0 start) (text-edit-replacement edit) (substring s end (string-length s))))))

  (define apply-edits
    (case-lambda
      [(s edits)
       (unless (string? s) (assertion-violationf 'apply-edits "~s is not a string" s))
       (apply-edits s 0 (string-length s) edits)]
      [(s from to edits)
       (unless (string? s) (assertion-violationf 'apply-edits "~s is not a string" s))
       (unless (and (fixnum? from) (fixnum? to) (fx<= 0 from to (string-length s)))
         (assertion-violationf 'apply-edits "~s to ~s is not a part of a string of length ~s" from to (string-length s)))
       (unless (and (list? edits) (for-all text-edit? edits)
                    (let loop ([edits edits] [at from])
                      (or (null? edits)
                          (and (fx<= at (text-edit-start (car edits))) (fx<= (text-edit-end (car edits)) to)
                               (loop (cdr edits) (text-edit-end (car edits)))))))
         (assertion-violationf 'apply-edits "~s is not a list of ordered edits within ~s to ~s" edits from to))
       (let-values ([(out get) (open-string-output-port)])
         (let loop ([edits edits] [at from])
           (if (null? edits)
               (begin (put-string out s at (fx- to at)) (get))
               (let ([e (car edits)])
                 (put-string out s at (fx- (text-edit-start e) at))
                 (put-string out (text-edit-replacement e))
                 (loop (cdr edits) (text-edit-end e))))))]))

  (define (text-difference-edit old new)
    (unless (string? old) (assertion-violationf 'text-difference-edit "~s is not a string" old))
    (unless (string? new) (assertion-violationf 'text-difference-edit "~s is not a string" new))
    (let* ([old-n (string-length old)]
           [new-n (string-length new)]
           [limit (fxmin old-n new-n)]
           [prefix (let loop ([i 0])
                     (if (and (fx< i limit) (char=? (string-ref old i) (string-ref new i)))
                         (loop (fx+ i 1))
                         i))]
           [suffix (let loop ([i 0])
                     (if (and (fx< i (fx- limit prefix))
                              (char=? (string-ref old (fx- old-n i 1)) (string-ref new (fx- new-n i 1))))
                         (loop (fx+ i 1))
                         i))])
      (make-text-edit prefix (fx- old-n suffix) (substring new prefix (fx- new-n suffix)))))

  ;; A map keeps only the intervals that changed, merged where they touch, so
  ;; its size is bounded by the text's length however many edits it records.
  (define-record-type offset-map
    (nongenerative) (sealed #t)
    (fields a-starts a-ends b-starts b-ends))

  (define identity-offset-map
    (make-offset-map (fxvector) (fxvector) (fxvector) (fxvector)))

  (define (offset-map-size m) (fxvector-length (offset-map-a-starts m)))

  (define (list->offset-map intervals)
    (let* ([n (length intervals)]
           [a-starts (make-fxvector n)] [a-ends (make-fxvector n)]
           [b-starts (make-fxvector n)] [b-ends (make-fxvector n)])
      (let loop ([intervals intervals] [i 0])
        (unless (null? intervals)
          (let ([iv (car intervals)])
            (fxvector-set! a-starts i (vector-ref iv 0))
            (fxvector-set! a-ends i (vector-ref iv 1))
            (fxvector-set! b-starts i (vector-ref iv 2))
            (fxvector-set! b-ends i (vector-ref iv 3))
            (loop (cdr intervals) (fx+ i 1)))))
      (make-offset-map a-starts a-ends b-starts b-ends)))

  (define (last-interval-at-or-before starts x)
    (let loop ([lo -1] [hi (fxvector-length starts)])
      (if (fx= (fx+ lo 1) hi)
          lo
          (let ([mid (fxsrl (fx+ lo hi) 1)])
            (if (fx<= (fxvector-ref starts mid) x)
                (loop mid hi)
                (loop lo mid))))))

  (define (check-map who m)
    (unless (offset-map? m) (assertion-violationf who "~s is not an offset map" m)))

  (define (offset-map-offset m offset bias)
    (check-map 'offset-map-offset m)
    (unless (and (fixnum? offset) (fx>= offset 0))
      (assertion-violationf 'offset-map-offset "~s is not an offset" offset))
    (unless (memq bias '(left right))
      (assertion-violationf 'offset-map-offset "~s is not an edge bias" bias))
    (map-offset m offset bias))

  (define (map-offset m offset bias)
    (let ([i (last-interval-at-or-before (offset-map-a-starts m) offset)])
      (if (fx< i 0)
          offset
          (let ([a-start (fxvector-ref (offset-map-a-starts m) i)]
                [a-end (fxvector-ref (offset-map-a-ends m) i)]
                [b-start (fxvector-ref (offset-map-b-starts m) i)]
                [b-end (fxvector-ref (offset-map-b-ends m) i)])
            (cond
              [(fx= offset a-start a-end) (if (eq? bias 'left) b-start b-end)]
              [(fx= offset a-start) b-start]
              [(fx< offset a-end) #f]
              [else (fx+ b-end (fx- offset a-end))])))))

  (define (offset-map-span m start end)
    (check-map 'offset-map-span m)
    (unless (and (fixnum? start) (fixnum? end) (fx<= 0 start end))
      (assertion-violationf 'offset-map-span "~s and ~s are not a span" start end))
    (let ([i (last-interval-at-or-before (offset-map-a-starts m) end)])
      (if (and (fx>= i 0) (fx>= (fxvector-ref (offset-map-a-ends m) i) start))
          (values #f #f)
          (values (map-offset m start 'left) (map-offset m end 'left)))))

  (define (offset-map-span-back m start end)
    (check-map 'offset-map-span-back m)
    (unless (and (fixnum? start) (fixnum? end) (fx<= 0 start end))
      (assertion-violationf 'offset-map-span-back "~s and ~s are not a span" start end))
    (let ([i (last-interval-at-or-before (offset-map-b-starts m) end)])
      (if (and (fx>= i 0) (fx>= (fxvector-ref (offset-map-b-ends m) i) start))
          (values #f #f)
          (values (unmap-offset m start) (unmap-offset m end)))))

  ;; Exact only for an offset outside every changed interval on the b side.
  (define (unmap-offset m b)
    (let ([i (last-interval-at-or-before (offset-map-b-starts m) b)])
      (if (fx< i 0)
          b
          (fx+ (fxvector-ref (offset-map-a-ends m) i)
               (fx- b (fxvector-ref (offset-map-b-ends m) i))))))

  ;; Merging both maps' changed intervals on the shared side, where they
  ;; touch, leaves each group's ends on interval ends or in unchanged text of
  ;; each map, so both ends map back and forward exactly.
  (define (offset-map-compose m1 m2)
    (check-map 'offset-map-compose m1)
    (check-map 'offset-map-compose m2)
    (let* ([groups (merge-touching
                     (merge-sorted
                       (let loop ([i (fx- (offset-map-size m1) 1)] [acc '()])
                         (if (fx< i 0)
                             acc
                             (loop (fx- i 1)
                                   (cons (cons (fxvector-ref (offset-map-b-starts m1) i)
                                               (fxvector-ref (offset-map-b-ends m1) i))
                                         acc))))
                       (let loop ([i (fx- (offset-map-size m2) 1)] [acc '()])
                         (if (fx< i 0)
                             acc
                             (loop (fx- i 1)
                                   (cons (cons (fxvector-ref (offset-map-a-starts m2) i)
                                               (fxvector-ref (offset-map-a-ends m2) i))
                                         acc))))))])
      (list->offset-map
        (map (lambda (g)
               (let ([b-start (car g)] [b-end (cdr g)])
                 (vector (back-start m1 b-start) (back-end m1 b-end)
                         (forward-start m2 b-start) (forward-end m2 b-end))))
             groups))))

  (define (back-start m b)
    (let ([i (last-interval-at-or-before (offset-map-b-starts m) b)])
      (if (and (fx>= i 0) (fx= (fxvector-ref (offset-map-b-starts m) i) b))
          (fxvector-ref (offset-map-a-starts m) i)
          (unmap-offset m b))))

  (define (back-end m b)
    (let ([i (last-interval-at-or-before (offset-map-b-starts m) b)])
      (if (and (fx>= i 0) (fx= (fxvector-ref (offset-map-b-ends m) i) b))
          (fxvector-ref (offset-map-a-ends m) i)
          (unmap-offset m b))))

  (define (forward-start m a)
    (let ([i (last-interval-at-or-before (offset-map-a-starts m) a)])
      (if (and (fx>= i 0) (fx= (fxvector-ref (offset-map-a-starts m) i) a))
          (fxvector-ref (offset-map-b-starts m) i)
          (map-offset m a 'left))))

  (define (forward-end m a)
    (let ([i (last-interval-at-or-before (offset-map-a-starts m) a)])
      (if (and (fx>= i 0) (fx= (fxvector-ref (offset-map-a-ends m) i) a))
          (fxvector-ref (offset-map-b-ends m) i)
          (map-offset m a 'right))))

  (define (merge-sorted xs ys)
    (cond
      [(null? xs) ys]
      [(null? ys) xs]
      [(fx<= (caar xs) (caar ys)) (cons (car xs) (merge-sorted (cdr xs) ys))]
      [else (cons (car ys) (merge-sorted xs (cdr ys)))]))

  (define (merge-touching intervals)
    (if (null? intervals)
        '()
        (let loop ([current (car intervals)] [rest (cdr intervals)] [acc '()])
          (cond
            [(null? rest) (reverse (cons current acc))]
            [(fx<= (caar rest) (cdr current))
             (loop (cons (car current) (fxmax (cdr current) (cdar rest))) (cdr rest) acc)]
            [else (loop (car rest) (cdr rest) (cons current acc))]))))

  (define (offset-map-add-edit m edit)
    (check-map 'offset-map-add-edit m)
    (unless (text-edit? edit) (assertion-violationf 'offset-map-add-edit "~s is not a text edit" edit))
    (let ([start (text-edit-start edit)] [end (text-edit-end edit)])
      (offset-map-compose m
        (list->offset-map
          (list (vector start end start (fx+ start (string-length (text-edit-replacement edit)))))))))

  (define (line-ending-offset-map from to)
    (unless (string? from) (assertion-violationf 'line-ending-offset-map "~s is not a string" from))
    (unless (string? to) (assertion-violationf 'line-ending-offset-map "~s is not a string" to))
    (let ([n (string-length from)] [m (string-length to)])
      (define (crlf? s i k) (and (fx< (fx+ i 1) k) (char=? (string-ref s i) #\return) (char=? (string-ref s (fx+ i 1)) #\newline)))
      (define (lf? s i k) (and (fx< i k) (char=? (string-ref s i) #\newline)))
      (let loop ([i 0] [j 0] [acc '()])
        (cond
          [(and (fx= i n) (fx= j m)) (list->offset-map (reverse acc))]
          [(and (crlf? from i n) (lf? to j m))
           (loop (fx+ i 2) (fx+ j 1) (cons (vector i (fx+ i 2) j (fx+ j 1)) acc))]
          [(and (lf? from i n) (crlf? to j m))
           (loop (fx+ i 1) (fx+ j 2) (cons (vector i (fx+ i 1) j (fx+ j 2)) acc))]
          [(and (fx< i n) (fx< j m) (char=? (string-ref from i) (string-ref to j)))
           (loop (fx+ i 1) (fx+ j 1) acc)]
          [else #f])))))
