(import (chezscheme) (check) (analysis text))

(random-seed 20261105)

(define alphabet
  (vector #\a #\b #\space #\newline #\return #\x3BB #\x1F600 #\x10000 #\x2028 #\tab))

(define (random-string n)
  (let ([s (make-string n)])
    (do ([i 0 (fx+ i 1)]) ((fx= i n) s)
      (string-set! s i (vector-ref alphabet (random (vector-length alphabet)))))))

(define (utf-16-length s start end)
  (let loop ([i start] [n 0])
    (if (fx= i end)
        n
        (loop (fx+ i 1) (+ n (if (fx>= (char->integer (string-ref s i)) #x10000) 2 1))))))

(test "lines end at LF, CRLF and CR"
  (let ([t (make-text "a\nb\r\nc\rd")])
    (check-equal? 4 (text-line-count t))
    (check-equal? '(0 2 5 7) (map (lambda (i) (text-line-start t i)) '(0 1 2 3)))
    (check-equal? '(1 3 6 8) (map (lambda (i) (text-line-end t i)) '(0 1 2 3)))
    (check-equal? '(0 0 1 1 1 2 2 3) (map (lambda (o) (text-offset-line t o)) '(0 1 2 3 4 5 6 7)))
    (check-equal? "b\r\n" (text-substring t 2 5)))
  (check-equal? 1 (text-line-count (make-text "")))
  (check-equal? 2 (text-line-count (make-text "x\n"))))

(test "offset to position to offset is the identity in both encodings"
  (do ([k 0 (fx+ k 1)]) ((fx= k 300))
    (let* ([s (random-string (random 60))] [t (make-text s)])
      (do ([o 0 (fx+ o 1)]) ((fx> o (string-length s)))
        (for-each
          (lambda (enc)
            (let-values ([(line character) (text-offset->position t enc o)])
              (unless (and (fx> o 0) (char=? (string-ref s (fx- o 1)) #\return)
                           (fx< o (string-length s)) (char=? (string-ref s o) #\newline))
                (check-equal? o (text-position->offset t enc line character))
                (check-equal? o (text-position->offset/strict t enc line character)))))
          '(utf-16 utf-32))))))

(test "request positions clamp; strict positions refuse"
  (let ([t (make-text "ab\n\x1F600;c\n")])
    (check-equal? 2 (text-position->offset t 'utf-32 0 10))
    (check-equal? #f (text-position->offset/strict t 'utf-32 0 10))
    (check-equal? 2 (text-position->offset/strict t 'utf-32 0 2))
    (check-equal? 6 (text-position->offset t 'utf-16 9 0))
    (check-equal? #f (text-position->offset/strict t 'utf-16 4 0))
    (check-equal? 6 (text-position->offset/strict t 'utf-16 3 0))
    (check-equal? 3 (text-position->offset t 'utf-16 1 1))
    (check-equal? #f (text-position->offset/strict t 'utf-16 1 1))
    (check-equal? 4 (text-position->offset t 'utf-16 1 2))
    (check-equal? 5 (text-position->offset t 'utf-16 1 3))
    (check-equal? #f (text-position->offset/strict t 'utf-16 1 4))
    (check-equal? 4 (text-position->offset t 'utf-32 1 1)))
  (check-raises assertion-violation? (text-position->offset (make-text "") 'utf-16 -1 0))
  (check-raises assertion-violation? (text-position->offset (make-text "") 'utf-8 0 0)))

(test "positions agree with a count of code units"
  (do ([k 0 (fx+ k 1)]) ((fx= k 200))
    (let* ([s (random-string (random 50))] [t (make-text s)])
      (do ([o 0 (fx+ o 1)]) ((fx> o (string-length s)))
        (let-values ([(line character) (text-offset->position t 'utf-16 o)])
          (check-equal? (utf-16-length s (text-line-start t line) o) character))))))

(define (random-edit len)
  (let* ([start (random (fx+ len 1))]
         [end (fx+ start (random (fx+ (fx- len start) 1)))])
    (make-text-edit start end (random-string (random 4)))))

(test "incremental edits equal the full replacement"
  (do ([k 0 (fx+ k 1)]) ((fx= k 200))
    (let loop ([t (make-text (random-string (random 40)))] [n 0])
      (when (fx< n 20)
        (let* ([e (random-edit (text-length t))]
               [s (text-string t)]
               [expected (string-append (substring s 0 (text-edit-start e))
                                        (text-edit-replacement e)
                                        (substring s (text-edit-end e) (string-length s)))]
               [t2 (text-apply-edit t e)])
          (check-equal? expected (text-string t2))
          (check-equal? (text-line-count (make-text expected)) (text-line-count t2))
          (check-equal? expected (text-string (text-apply-edit t (text-difference-edit s expected))))
          (loop t2 (fx+ n 1))))))
  (check-raises assertion-violation? (text-apply-edit (make-text "ab") (make-text-edit 1 3 "")))
  (check-raises assertion-violation? (make-text-edit 2 1 "")))

(test "apply-edits applies ordered edits at once, to a whole string or a part of it"
  (let ([edits (list (make-text-edit 0 0 "<") (make-text-edit 1 3 "x") (make-text-edit 3 3 "y") (make-text-edit 5 6 ""))])
    (check-equal? "<axyde" (apply-edits "abcdef" edits))
    (check-equal? "xyd" (apply-edits "abcdef" 1 4 (cdr (reverse (cdr (reverse edits))))))
    (check-equal? "abc" (apply-edits "abc" '())))
  (check-raises assertion-violation? (apply-edits "abc" (list (make-text-edit 2 3 "") (make-text-edit 0 1 ""))))
  (check-raises assertion-violation? (apply-edits "abc" (list (make-text-edit 2 4 ""))))
  (check-raises assertion-violation? (apply-edits "abcdef" 1 4 (list (make-text-edit 0 1 ""))))
  (check-raises assertion-violation? (apply-edits "abcdef" 1 4 (list (make-text-edit 3 5 ""))))
  (check-raises assertion-violation? (apply-edits "abc" 2 1 '())))

;; Tracks every offset of the first version through the edits one by one.
;; An offset is touched when it ever sits on or inside an edit. Untouched
;; offsets have one right answer, and so does an offset whose first edit
;; contains it strictly: none.
(define (brute-force-track len edits)
  (let ([pos (make-vector (fx+ len 1))] [touched (make-vector (fx+ len 1) #f)]
        [lost (make-vector (fx+ len 1) #f)])
    (do ([i 0 (fx+ i 1)]) ((fx> i len)) (vector-set! pos i i))
    (for-each
      (lambda (e)
        (let ([s (text-edit-start e)] [end (text-edit-end e)]
              [delta (fx- (string-length (text-edit-replacement e)) (fx- (text-edit-end e) (text-edit-start e)))])
          (do ([i 0 (fx+ i 1)]) ((fx> i len))
            (let ([x (vector-ref pos i)])
              (when x
                (cond
                  [(fx< x s) (void)]
                  [(fx> x end) (vector-set! pos i (fx+ x delta))]
                  [(and (fx< s x) (fx< x end))
                   (unless (vector-ref touched i) (vector-set! lost i #t))
                   (vector-set! pos i #f)
                   (vector-set! touched i #t)]
                  [else
                   (vector-set! touched i #t)
                   (when (fx= x end) (vector-set! pos i (fx+ x delta)))]))))))
      edits)
    (values pos touched lost)))

(define (check-map-against-brute-force len edits m)
  (let-values ([(pos touched lost) (brute-force-track len edits)])
    (do ([i 0 (fx+ i 1)]) ((fx> i len))
      (let ([left (offset-map-offset m i 'left)] [right (offset-map-offset m i 'right)])
        (cond
          [(not (vector-ref touched i))
           (check-equal? (vector-ref pos i) left)
           (check-equal? (vector-ref pos i) right)]
          [(vector-ref lost i)
           (check-equal? #f left)
           (check-equal? #f right)])))))

(test "compacted offset maps agree with tracking every offset"
  (do ([k 0 (fx+ k 1)]) ((fx= k 300))
    (let loop ([t (make-text (random-string (random 40)))] [m identity-offset-map] [edits '()] [n 0]
               [len0 #f])
      (let ([len0 (or len0 (text-length t))])
        (if (fx= n 15)
            (check-map-against-brute-force len0 (reverse edits) m)
            (let ([e (random-edit (text-length t))])
              (loop (text-apply-edit t e) (offset-map-add-edit m e) (cons e edits) (fx+ n 1) len0)))))))

(test "composing maps equals adding their edits in turn"
  (do ([k 0 (fx+ k 1)]) ((fx= k 200))
    (let* ([t0 (make-text (random-string (random 40)))]
           [e1 (random-edit (text-length t0))]
           [t1 (text-apply-edit t0 e1)]
           [e2 (random-edit (text-length t1))]
           [e3 (random-edit (text-length (text-apply-edit t1 e2)))]
           [m12 (offset-map-add-edit (offset-map-add-edit identity-offset-map e1) e2)]
           [composed (offset-map-compose m12 (offset-map-add-edit identity-offset-map e3))]
           [direct (offset-map-add-edit m12 e3)])
      (do ([i 0 (fx+ i 1)]) ((fx> i (text-length t0)))
        (check-equal? (offset-map-offset direct i 'left) (offset-map-offset composed i 'left))
        (check-equal? (offset-map-offset direct i 'right) (offset-map-offset composed i 'right))))))

;; The map's representation is private; its first field holds one start per
;; interval.
(define (offset-map-interval-count m)
  (fxvector-length ((record-accessor (record-rtd m) 0) m)))

(test "a map's size stays bounded by the text's length over many edits"
  (let loop ([t (make-text (random-string 200))] [m identity-offset-map] [n 0] [largest 0])
    (if (fx= n 100000)
        (check (fx<= largest (fx+ (text-length t) 1)))
        (let* ([e (random-edit (text-length t))]
               [t2 (text-apply-edit t e)]
               [m2 (offset-map-add-edit m e)]
               [size (offset-map-interval-count m2)])
          (unless (fx<= size (fx+ (text-length t2) 1))
            (check-equal? 'bounded (list size (text-length t2))))
          (loop (if (fx> (text-length t2) 400) (make-text (random-string 200)) t2)
                (if (fx> (text-length t2) 400) identity-offset-map m2)
                (fx+ n 1)
                (fxmax largest size))))))

(test "a span touched at either end maps to nothing"
  (let ([m (offset-map-add-edit identity-offset-map (make-text-edit 13 13 "b"))])
    (let-values ([(s e) (offset-map-span m 10 13)]) (check-equal? '(#f #f) (list s e)))
    (let-values ([(s e) (offset-map-span m 13 16)]) (check-equal? '(#f #f) (list s e)))
    (let-values ([(s e) (offset-map-span m 14 16)]) (check-equal? '(15 17) (list s e)))
    (let-values ([(s e) (offset-map-span m 2 12)]) (check-equal? '(2 12) (list s e)))
    (check-equal? 13 (offset-map-offset m 13 'left))
    (check-equal? 14 (offset-map-offset m 13 'right))))

(test "the line-ending map agrees with a brute-force mapping"
  (do ([k 0 (fx+ k 1)]) ((fx= k 300))
    (let* ([lines (map (lambda (i) (random-string (random 5))) (iota (random 6)))]
           [clean (map (lambda (l) (list->string (remp (lambda (c) (memv c '(#\newline #\return))) (string->list l)))) lines)]
           [endings (map (lambda (l) (if (fx= (random 2) 0) "\r\n" "\n")) clean)]
           [from (apply string-append (map string-append clean endings))]
           [to (apply string-append (map (lambda (l) (string-append l "\n")) clean))]
           [m (line-ending-offset-map from to)])
      (let loop ([ls clean] [es endings] [i 0] [j 0])
        (unless (null? ls)
          (let ([n (string-length (car ls))])
            (do ([k 0 (fx+ k 1)]) ((fx> k n))
              (check-equal? (fx+ j k) (offset-map-offset m (fx+ i k) 'left)))
            (when (string=? (car es) "\r\n")
              (check-equal? #f (offset-map-offset m (fx+ i n 1) 'left)))
            (loop (cdr ls) (cdr es) (fx+ i n (string-length (car es))) (fx+ j n 1)))))))
  (check-equal? #f (line-ending-offset-map "a\nb" "a\nc"))
  (check-equal? #f (line-ending-offset-map "a\rb" "a\nb")))

(define (transcoder-decode bv)
  (get-string-all (transcoded-port (open-bytevector-input-port bv) (current-transcoder))))

(test "decoding source bytes equals Chez's source transcoder"
  (for-each
    (lambda (bv)
      (let ([expected (transcoder-decode bv)])
        (check-equal? (if (eof-object? expected) "" expected) (decode-source-bytes bv))))
    (list #vu8(#xEF #xBB #xBF #x28 #x61 #x29 #x0D #x0A)
          #vu8(#x61 #xFF #x62)
          #vu8(#xE2 #x82)
          #vu8()
          (string->utf8 "λ\r\n\x1F600;")))
  (do ([k 0 (fx+ k 1)]) ((fx= k 2000))
    (let ([bv (make-bytevector (random 12))])
      (do ([i 0 (fx+ i 1)]) ((fx= i (bytevector-length bv)))
        (bytevector-u8-set! bv i (if (fx= (random 3) 0) (random 256) (fx+ #x80 (random 64)))))
      (let ([expected (transcoder-decode bv)])
        (check-equal? (if (eof-object? expected) "" expected) (decode-source-bytes bv))))))

(define (bignum-fnv1a s)
  (let loop ([bytes (bytevector->u8-list (string->utf8 s))] [h #xcbf29ce484222325])
    (if (null? bytes)
        (let ([hex (string-downcase (number->string h 16))])
          (string-append (make-string (fx- 16 (string-length hex)) #\0) hex))
        (loop (cdr bytes) (mod (* (logxor h (car bytes)) #x100000001b3) (expt 2 64))))))

(test "the text hash equals FNV-1a computed on bignums"
  (check-equal? "cbf29ce484222325" (text-hash (make-text "")))
  (do ([k 0 (fx+ k 1)]) ((fx= k 500))
    (let ([s (random-string (random 30))])
      (check-equal? (bignum-fnv1a s) (text-hash (make-text s)))))
  (let ([t (make-text "stable")])
    (check (eq? (text-hash t) (text-hash t)))
    (check-equal? (text-hash t) (text-hash (make-text "stable")))))

(test "the text hash differs across a one-character edit"
  (let ([collisions 0])
    (do ([k 0 (fx+ k 1)]) ((fx= k 1000000))
      (let* ([s (random-string (fx+ 1 (random 20)))]
             [i (random (string-length s))]
             [c (vector-ref alphabet (random (vector-length alphabet)))]
             [s2 (string-copy s)])
        (string-set! s2 i c)
        (unless (or (string=? s s2)
                    (not (string=? (text-hash (make-text s)) (text-hash (make-text s2)))))
          (set! collisions (fx+ collisions 1)))))
    (check-equal? 0 collisions)))

(test-exit)
