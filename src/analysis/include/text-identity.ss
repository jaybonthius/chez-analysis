;; Chez decodes source files with this transcoder: a leading byte-order mark
;; is skipped and not counted, and line endings are kept, so offsets computed
;; here agree with the positions Chez's reader reports for the same file.
(define source-transcoder
  (make-transcoder (utf-8-codec) (eol-style none) (error-handling-mode replace)))

(define (decode-source-bytes bv)
  (unless (bytevector? bv)
    (assertion-violationf 'decode-source-bytes "~s is not a bytevector" bv))
  (bytevector->string bv source-transcoder))

;; FNV-1a over 64 bits, computed on two 32-bit lanes because a 64-bit value
;; is a bignum in Chez and bignum arithmetic is ten times slower. The prime is
;; 2^40 + #x1b3, so a step multiplies the low lane by #x1b3 and carries the
;; low lane, shifted by 8, into the high lane.
(define (fnv-step hi lo byte)
  (let* ([lo (fxlogxor lo byte)]
         [m (fx* lo #x1b3)])
    (values (fxlogand (fx+ (fx* hi #x1b3) (fxsll lo 8) (fxsrl m 32)) #xffffffff)
            (fxlogand m #xffffffff))))

(define fnv-offset-hi #xcbf29ce4)
(define fnv-offset-lo #x84222325)

(define (fnv-hex hi lo)
  (string->immutable-string (string-append (hex-lane hi) (hex-lane lo))))

(define (source-text-hash s)
  (let ([n (string-length s)])
    (let loop ([i 0] [hi fnv-offset-hi] [lo fnv-offset-lo])
      (if (fx= i n)
          (fnv-hex hi lo)
          (let ([c (char->integer (string-ref s i))])
            (cond
              [(fx< c #x80)
               (let-values ([(hi lo) (fnv-step hi lo c)])
                 (loop (fx+ i 1) hi lo))]
              [(fx< c #x800)
               (let*-values ([(hi lo) (fnv-step hi lo (fxlogor #xc0 (fxsrl c 6)))]
                             [(hi lo) (fnv-step hi lo (fxlogor #x80 (fxlogand c #x3f)))])
                 (loop (fx+ i 1) hi lo))]
              [(fx< c #x10000)
               (let*-values ([(hi lo) (fnv-step hi lo (fxlogor #xe0 (fxsrl c 12)))]
                             [(hi lo) (fnv-step hi lo (fxlogor #x80 (fxlogand (fxsrl c 6) #x3f)))]
                             [(hi lo) (fnv-step hi lo (fxlogor #x80 (fxlogand c #x3f)))])
                 (loop (fx+ i 1) hi lo))]
              [else
               (let*-values ([(hi lo) (fnv-step hi lo (fxlogor #xf0 (fxsrl c 18)))]
                             [(hi lo) (fnv-step hi lo (fxlogor #x80 (fxlogand (fxsrl c 12) #x3f)))]
                             [(hi lo) (fnv-step hi lo (fxlogor #x80 (fxlogand (fxsrl c 6) #x3f)))]
                             [(hi lo) (fnv-step hi lo (fxlogor #x80 (fxlogand c #x3f)))])
                 (loop (fx+ i 1) hi lo))]))))))

(define (bytevector-hash bv)
  (unless (bytevector? bv)
    (assertion-violationf 'bytevector-hash "~s is not a bytevector" bv))
  (let ([n (bytevector-length bv)])
    (let loop ([i 0] [hi fnv-offset-hi] [lo fnv-offset-lo])
      (if (fx= i n)
          (fnv-hex hi lo)
          (let-values ([(hi lo) (fnv-step hi lo (bytevector-u8-ref bv i))])
            (loop (fx+ i 1) hi lo))))))

(define (hex-lane x)
  (let ([s (number->string x 16)])
    (string-downcase
      (string-append (make-string (fx- 8 (string-length s)) #\0) s))))
