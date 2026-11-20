;; Classifies a number-like token as Chez's $str->num does, without computing
;; the number: Chez computes an exact number with a large exponent in full
;; (#e1e1000000000 does not finish), while a token's kind and reader errors
;; depend only on the outcome. The states and their order follow s/strnum.ss
;; one for one. Only zero-ness matters of a part's value, because Chez's
;; outcome tests only for exact 0; big marks an exact value too costly to
;; compute, which is never a fixnum, flonum or octet.
(define (classify-number s start end r6rs?)
  ;; Bounds past which an exact exponent or mantissa width makes the value
  ;; unbounded in size, while no fixnum can result: the mantissa has fewer
  ;; digits than the token has characters.
  (define exponent-bound (fx+ (fx- end start) 70))
  (define width-bound (fx+ (fx* 4 (fx- end start)) 70))
  (define saturation (expt 2 40))

  (define (char-at i) (char-downcase (string-ref s i)))

  (define (digit c r)
    (let ([v (cond
               [(char<=? #\0 c #\9) (fx- (char->integer c) 48)]
               [(char<=? #\a c #\z) (fx- (char->integer c) 87)]
               [else 36])])
      (and (fx< v r) v)))

  (define (saturate x) (if (> x saturation) saturation x))

  (define (at-end bang v) (if (and r6rs? bang) '!r6rs v))

  ;; make-part.
  (define (part inexact? nonzero?) (if inexact? 'value (if nonzero? 'value 'zero)))

  ;; e is the exponent's magnitude: either sign makes an exact value costly.
  (define (exponent-part inexact? wi e)
    (case wi
      [(nan inf) (if inexact? 'value 'norep)]
      [else (cond
              [inexact? 'value]
              [(eq? wi 'zero) 'zero]
              [(> e exponent-bound) 'big]
              [else 'value])]))

  (define (mkrat inexact? d-inexact? nan inf p-nonzero? q-nonzero?)
    (if (not q-nonzero?)
        (if d-inexact? 'value (if p-nonzero? inf nan))
        (if (or inexact? d-inexact?) 'value (if p-nonzero? 'value 'zero))))

  (define (finish-number ms ex x1 x)
    (case ms
      [(real ureal) x]
      [(angle)
       (cond
         [(or (eq? x1 'norep) (eq? x 'norep)) 'norep]
         [(eq? x1 'zero) 'zero]
         [(eq? x 'zero) x1]
         [(eq? ex 'e) 'norep]
         [else 'value])]
      [else #f]))

  (define (finish-rectangular ms x1 x)
    (case ms
      [(real ureal) (if (eq? x 'norep) 'norep (if (eq? x 'big) 'big (if (eq? x 'zero) 'zero 'value)))]
      [(imag)
       (cond
         [(or (eq? x1 'norep) (eq? x 'norep)) 'norep]
         [(eq? x 'zero) x1]
         [(or (eq? x1 'big) (eq? x 'big)) 'big]
         [else 'value])]
      [else #f]))

  ;; A mantissa-width rounding of an exact value is cheap when the value is
  ;; an integer, and otherwise costs a shift by the width.
  (define (width-part ex nonzero? integer? mw)
    (cond
      [(not (eq? ex 'e)) 'value]
      [(eqv? mw 0) 'zero]
      [(not nonzero?) 'zero]
      [(and (not integer?) (> mw width-bound)) 'big]
      [else 'value]))

  (define (width-exponent-part ex wi e mw)
    (let ([x (exponent-part (not (eq? ex 'e)) wi e)])
      (if (and (eq? x 'value) (eq? ex 'e) (> mw width-bound)) 'big x)))

  (define (exponent-marker? c) (memv c '(#\e #\s #\f #\d #\l)))
  (define (r6rs-radix bang r) (or bang (not (fx= r 10))))

  (define (prefix0 i bang x1 r ex)
    (cond
      [(fx= i end) #f]
      [(char=? (char-at i) #\#) (prefix1 (fx+ i 1) bang x1 r ex)]
      [else (num0 i bang x1 r ex)]))

  (define (prefix1 i bang x1 r ex)
    (if (fx= i end)
        #f
        (let ([c (char-at i)])
          (cond
            [(digit c 10) => (lambda (d) (prefix2 (fx+ i 1) #t x1 d ex))]
            [(char=? c #\e) (prefix3 (fx+ i 1) bang x1 r 'e)]
            [(char=? c #\i) (prefix3 (fx+ i 1) bang x1 r 'i)]
            [(char=? c #\b) (prefix6 (fx+ i 1) bang x1 2 ex)]
            [(char=? c #\o) (prefix6 (fx+ i 1) bang x1 8 ex)]
            [(char=? c #\d) (prefix6 (fx+ i 1) bang x1 10 ex)]
            [(char=? c #\x) (prefix6 (fx+ i 1) bang x1 16 ex)]
            [else 'bogus]))))

  (define (prefix2 i bang x1 r ex)
    (if (fx= i end)
        #f
        (let* ([c (char-at i)] [d (digit c 10)])
          (cond
            [(and d (fx< r 37)) (prefix2 (fx+ i 1) bang x1 (fx+ (fx* r 10) d) ex)]
            [(and (char=? c #\r) (fx< 1 r 37)) (prefix6 (fx+ i 1) bang x1 r ex)]
            [else 'bogus]))))

  (define (prefix3 i bang x1 r ex)
    (cond
      [(fx= i end) #f]
      [(char=? (char-at i) #\#) (prefix4 (fx+ i 1) bang x1 ex)]
      [else (num0 i bang x1 r ex)]))

  (define (prefix4 i bang x1 ex)
    (if (fx= i end)
        #f
        (let ([c (char-at i)])
          (cond
            [(digit c 10) => (lambda (d) (prefix5 (fx+ i 1) #t x1 d ex))]
            [(char=? c #\b) (num0 (fx+ i 1) bang x1 2 ex)]
            [(char=? c #\o) (num0 (fx+ i 1) bang x1 8 ex)]
            [(char=? c #\d) (num0 (fx+ i 1) bang x1 10 ex)]
            [(char=? c #\x) (num0 (fx+ i 1) bang x1 16 ex)]
            [else 'bogus]))))

  (define (prefix5 i bang x1 r ex)
    (if (fx= i end)
        #f
        (let* ([c (char-at i)] [d (digit c 10)])
          (cond
            [(and d (fx< r 37)) (prefix5 (fx+ i 1) bang x1 (fx+ (fx* r 10) d) ex)]
            [(and (char=? c #\r) (fx< 1 r 37)) (num0 (fx+ i 1) bang x1 r ex)]
            [else 'bogus]))))

  (define (prefix6 i bang x1 r ex)
    (cond
      [(fx= i end) #f]
      [(char=? (char-at i) #\#) (prefix7 (fx+ i 1) bang x1 r)]
      [else (num0 i bang x1 r ex)]))

  (define (prefix7 i bang x1 r)
    (if (fx= i end)
        #f
        (let ([c (char-at i)])
          (cond
            [(char=? c #\e) (num0 (fx+ i 1) bang x1 r 'e)]
            [(char=? c #\i) (num0 (fx+ i 1) bang x1 r 'i)]
            [else 'bogus]))))

  (define (num0 i bang x1 r ex)
    (if (fx= i end)
        #f
        (let ([c (char-at i)])
          (cond
            [(digit c r) => (lambda (d) (num2 (fx+ i 1) bang x1 r ex 'ureal (fx> d 0)))]
            [(char=? c #\.) (float0 (fx+ i 1) (r6rs-radix bang r) x1 r ex 'ureal)]
            [(char=? c #\+) (num1 (fx+ i 1) bang x1 r ex 'real)]
            [(char=? c #\-) (num1 (fx+ i 1) bang x1 r ex 'real)]
            [else 'bogus]))))

  (define (num1 i bang x1 r ex ms)
    (if (fx= i end)
        #f
        (let ([c (char-at i)])
          (cond
            [(digit c r) => (lambda (d) (num2 (fx+ i 1) bang x1 r ex ms (fx> d 0)))]
            [(char=? c #\.) (float0 (fx+ i 1) (r6rs-radix bang r) x1 r ex ms)]
            [(char=? c #\i) (num3 (fx+ i 1) bang x1 r ex ms)]
            [(char=? c #\n) (nan0 (fx+ i 1) bang x1 r ex ms)]
            [else 'bogus]))))

  (define (num2 i bang x1 r ex ms nz)
    (if (fx= i end)
        (at-end bang (finish-number ms ex x1 (part (eq? ex 'i) nz)))
        (let ([c (char-at i)])
          (cond
            [(digit c r) => (lambda (d) (num2 (fx+ i 1) bang x1 r ex ms (or nz (fx> d 0))))]
            [(char=? c #\/) (rat0 (fx+ i 1) bang x1 r ex ex ms nz)]
            [(char=? c #\|) (mwidth0 (fx+ i 1) bang x1 r ex ms
                              (lambda (mw) (width-part ex nz #t mw)))]
            [(char=? c #\.) (float1 (fx+ i 1) (r6rs-radix bang r) x1 r ex ms nz #f)]
            [(char=? c #\#) (numhash (fx+ i 1) #t x1 r ex ms nz)]
            [(exponent-marker? c) (exp0 (fx+ i 1) (r6rs-radix bang r) x1 r ex ms (if nz 'value 'zero))]
            [else (complex0 i bang x1 r ex ms (part (eq? ex 'i) nz))]))))

  (define (num3 i bang x1 r ex ms)
    (cond
      [(fx= i end) (at-end bang (finish-rectangular ms x1 (part (eq? ex 'i) #t)))]
      [(char=? (char-at i) #\n) (inf0 (fx+ i 1) bang x1 r ex ms)]
      [else 'bogus]))

  (define (expect ch next)
    (lambda (i bang x1 r ex ms)
      (cond
        [(fx= i end) #f]
        [(char=? (char-at i) ch) (next (fx+ i 1) bang x1 r ex ms)]
        [else 'bogus])))

  (define (inf3 i bang x1 r ex ms)
    (let ([x (if (eq? ex 'e) 'norep 'value)])
      (if (fx= i end)
          (at-end bang (finish-number ms ex x1 x))
          (complex0 i bang x1 r ex ms x))))

  (define inf0 (expect #\f (expect #\. (expect #\0 inf3))))

  (define (nan4 i bang x1 r ex ms)
    (if (fx= i end)
        (at-end bang (finish-number ms ex x1 (if (eq? ex 'e) 'norep 'value)))
        (complex0 i bang x1 r ex ms 'value)))

  (define nan0 (expect #\a (expect #\n (expect #\. (expect #\0 nan4)))))

  (define (numhash i bang x1 r ex ms nz)
    (if (fx= i end)
        (at-end bang (finish-number ms ex x1 (part (not (eq? ex 'e)) nz)))
        (let ([c (char-at i)])
          (cond
            [(char=? c #\/) (rat0 (fx+ i 1) bang x1 r (if ex ex 'i) ex ms nz)]
            [(char=? c #\.) (floathash (fx+ i 1) bang x1 r ex ms nz #f)]
            [(char=? c #\#) (numhash (fx+ i 1) bang x1 r ex ms nz)]
            [(exponent-marker? c) (exp0 (fx+ i 1) bang x1 r ex ms (if nz 'value 'zero))]
            [else (complex0 i bang x1 r ex ms (part (not (eq? ex 'e)) nz))]))))

  (define (rat0 i bang x1 r ex d-ex ms m)
    (cond
      [(fx= i end) #f]
      [(digit (char-at i) r) => (lambda (d) (rat1 (fx+ i 1) bang x1 r ex d-ex ms m (fx> d 0)))]
      [else 'bogus]))

  (define (rat1 i bang x1 r ex d-ex ms m n)
    (if (fx= i end)
        (at-end bang (finish-number ms ex x1 (mkrat (eq? ex 'i) (eq? d-ex 'i) 'norep 'norep m n)))
        (let ([c (char-at i)])
          (cond
            [(digit c r) => (lambda (d) (rat1 (fx+ i 1) bang x1 r ex d-ex ms m (or n (fx> d 0))))]
            [(char=? c #\#) (rathash (fx+ i 1) #t x1 r ex ms m n)]
            [(exponent-marker? c) (exp0 (fx+ i 1) #t x1 r ex ms (mkrat #f #f 'nan 'inf m n))]
            [else (complex0 i bang x1 r ex ms (mkrat #f (eq? d-ex 'i) 'norep 'norep m n))]))))

  (define (rathash i bang x1 r ex ms m n)
    (if (fx= i end)
        (at-end bang (finish-number ms ex x1 (mkrat #f (not (eq? ex 'e)) 'norep 'norep m n)))
        (let ([c (char-at i)])
          (cond
            [(char=? c #\#) (rathash (fx+ i 1) bang x1 r ex ms m n)]
            [(exponent-marker? c) (exp0 (fx+ i 1) bang x1 r ex ms (mkrat #f #f 'nan 'inf m n))]
            [else (complex0 i bang x1 r ex ms (mkrat #f (not (eq? ex 'e)) 'norep 'norep m n))]))))

  (define (float0 i bang x1 r ex ms)
    (cond
      [(fx= i end) #f]
      [(digit (char-at i) r) => (lambda (d) (float1 (fx+ i 1) bang x1 r ex ms #f (fx> d 0)))]
      [else 'bogus]))

  (define (float1 i bang x1 r ex ms m-nz f-nz)
    (if (fx= i end)
        (at-end bang (finish-number ms ex x1 (part (not (eq? ex 'e)) (or m-nz f-nz))))
        (let ([c (char-at i)])
          (cond
            [(digit c r) => (lambda (d) (float1 (fx+ i 1) bang x1 r ex ms m-nz (or f-nz (fx> d 0))))]
            [(char=? c #\|) (mwidth0 (fx+ i 1) bang x1 r ex ms
                              (lambda (mw) (width-part ex (or m-nz f-nz) (not f-nz) mw)))]
            [(char=? c #\#) (floathash (fx+ i 1) #t x1 r ex ms m-nz f-nz)]
            [(exponent-marker? c) (exp0 (fx+ i 1) bang x1 r ex ms (if (or m-nz f-nz) 'value 'zero))]
            [else (complex0 i bang x1 r ex ms (part (not (eq? ex 'e)) (or m-nz f-nz)))]))))

  (define (floathash i bang x1 r ex ms m-nz f-nz)
    (if (fx= i end)
        (at-end bang (finish-number ms ex x1 (part (not (eq? ex 'e)) (or m-nz f-nz))))
        (let ([c (char-at i)])
          (cond
            [(char=? c #\#) (floathash (fx+ i 1) bang x1 r ex ms m-nz f-nz)]
            [(exponent-marker? c) (exp0 (fx+ i 1) bang x1 r ex ms (if (or m-nz f-nz) 'value 'zero))]
            [else (complex0 i bang x1 r ex ms (part (not (eq? ex 'e)) (or m-nz f-nz)))]))))

  (define (exp0 i bang x1 r ex ms wi)
    (if (fx= i end)
        #f
        (let ([c (char-at i)])
          (cond
            [(digit c r) => (lambda (d) (exp2 (fx+ i 1) bang x1 r ex ms wi d))]
            [(char=? c #\+) (exp1 (fx+ i 1) bang x1 r ex ms wi)]
            [(char=? c #\-) (exp1 (fx+ i 1) bang x1 r ex ms wi)]
            [else 'bogus]))))

  (define (exp1 i bang x1 r ex ms wi)
    (cond
      [(fx= i end) #f]
      [(digit (char-at i) r) => (lambda (d) (exp2 (fx+ i 1) bang x1 r ex ms wi d))]
      [else 'bogus]))

  (define (exp2 i bang x1 r ex ms wi e)
    (if (fx= i end)
        (at-end bang (finish-number ms ex x1 (exponent-part (not (eq? ex 'e)) wi e)))
        (let ([c (char-at i)])
          (cond
            [(digit c r) => (lambda (d) (exp2 (fx+ i 1) bang x1 r ex ms wi (saturate (+ (* r e) d))))]
            [(char=? c #\|) (mwidth0 (fx+ i 1) bang x1 r ex ms
                              (lambda (mw) (width-exponent-part ex wi e mw)))]
            [else (complex0 i bang x1 r ex ms (exponent-part (not (eq? ex 'e)) wi e))]))))

  (define (mwidth0 i bang x1 r ex ms x)
    (cond
      [(fx= i end) #f]
      [(digit (char-at i) 10) => (lambda (d) (mwidth1 (fx+ i 1) bang x1 r ex ms d x))]
      [else 'bogus]))

  (define (mwidth1 i bang x1 r ex ms mw x)
    (if (fx= i end)
        (at-end bang (finish-number ms ex x1 (x mw)))
        (let ([c (char-at i)])
          (cond
            [(digit c 10) => (lambda (d) (mwidth1 (fx+ i 1) bang x1 r ex ms (saturate (+ (* mw 10) d)) x))]
            [else (complex0 i bang x1 r ex ms (x mw))]))))

  (define (complex0 i bang x1 r ex ms x)
    (let ([c (char-at i)])
      (cond
        [(and (char=? c #\@) (memq ms '(real ureal))) (complex1 (fx+ i 1) bang x r ex 'angle)]
        [(and (char=? c #\+) (memq ms '(real ureal))) (num1 (fx+ i 1) bang x r ex 'imag)]
        [(and (char=? c #\-) (memq ms '(real ureal))) (num1 (fx+ i 1) bang x r ex 'imag)]
        [(and (char=? c #\i) (memq ms '(real imag))) (complex2 (fx+ i 1) bang x1 ms x)]
        [else 'bogus])))

  (define (complex1 i bang x1 r ex ms)
    (if (fx= i end)
        #f
        (let ([c (char-at i)])
          (cond
            [(digit c r) => (lambda (d) (num2 (fx+ i 1) bang x1 r ex ms (fx> d 0)))]
            [(char=? c #\.) (float0 (fx+ i 1) (r6rs-radix bang r) x1 r ex ms)]
            [(char=? c #\+) (num1 (fx+ i 1) bang x1 r ex ms)]
            [(char=? c #\-) (num1 (fx+ i 1) bang x1 r ex ms)]
            [else 'bogus]))))

  (define (complex2 i bang x1 ms x)
    (if (fx= i end)
        (at-end bang (finish-rectangular ms x1 x))
        'bogus))

  (let ([outcome (prefix0 start #f #f 10 #f)])
    (case outcome
      [(#f bogus !r6rs norep) (values outcome #f)]
      [else (values 'number outcome)])))
