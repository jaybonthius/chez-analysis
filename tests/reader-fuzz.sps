(import (chezscheme) (reader-oracle) (analysis text) (analysis reader) (analysis diagnostic))

(define mutant-count
  (let ([v (getenv "CHEZ_TOOLS_MUTANTS")]) (if v (string->number v) 1000000)))

(random-seed 20261106)

(define (random-element items) (list-ref items (random (length items))))

;; Chez's reader computes an exact number with a huge exponent in full and
;; allocates a sized vector before reading it; a mutant that writes either
;; would stall the comparison, not test the reader.
(define (costly-for-chez? c)
  (define (digit-run text)
    (let loop ([i 0] [run 0] [longest 0])
      (if (fx= i (string-length text))
          longest
          (if (char<=? #\0 (string-ref text i) #\9)
              (loop (fx+ i 1) (fx+ run 1) (fxmax longest (fx+ run 1)))
              (loop (fx+ i 1) 0 longest)))))
  (define (exact-prefix? text)
    (let loop ([i 0])
      (and (fx< (fx+ i 1) (string-length text))
           (or (and (char=? (string-ref text i) #\#) (char-ci=? (string-ref text (fx+ i 1)) #\e))
               (loop (fx+ i 1))))))
  (exists
    (lambda (t)
      (let ([text (token-text c t)])
        (case (token-kind t)
          [(open) (fx>= (digit-run text) 7)]
          [(number error) (and (exact-prefix? text) (fx>= (digit-run text) 6))]
          [else #f])))
    (vector->list (cst-tokens c))))

(define failures 0)
(define shown 0)

(define (run-mutants files)
  (let ([sources (list->vector (map (lambda (p) (let ([s (read-source-file p)]) (cons s (line-starts s)))) files))]
        [skipped 0] [read 0] [exceptions 0])
    (do ([k 0 (fx+ k 1)]) ((fx= k mutant-count))
      (let* ([source (vector-ref sources (random (vector-length sources)))]
             [mutant (random-mutant (car source) (cdr source))]
             [syntax (if (fx= (random 2) 0) 'chezscheme 'r6rs)]
             [fold (if (fx= (random 4) 0) 'fold 'no-fold)]
             [c (guard (e [#t #f]) (read-cst (make-text mutant) (make-reader-mode syntax fold)))])
        (cond
          [(not c)
           (set! exceptions (fx+ exceptions 1))
           (set! failures (fx+ failures 1))
           (when (fx< shown 40)
             (set! shown (fx+ shown 1))
             (printf "FAIL ~s ~a: read-cst raised\n" mutant (list syntax fold)))]
          [(costly-for-chez? c)
           (set! skipped (fx+ skipped 1))
           (let ([round-trip (round-trip-failure c mutant)])
             (when round-trip
               (set! failures (fx+ failures 1))
               (printf "FAIL ~s ~a [round-trip]: ~a\n" mutant (list syntax fold) round-trip)))]
          [else
           (let ([comparison (compare-with-chez mutant syntax fold)])
             (when (comparison-chez-read? comparison) (set! read (fx+ read 1)))
             (for-each
               (lambda (f)
                 (set! failures (fx+ failures 1))
                 (when (fx< shown 40)
                   (set! shown (fx+ shown 1))
                   (printf "FAIL ~s ~a [~a]: ~a\n" mutant (list syntax fold) (car f) (cdr f))))
               (comparison-failures comparison)))])))
    (printf "~a mutants: ~a read by Chez, ~a not given to Chez as too costly for it, ~a exceptions\n"
      mutant-count read skipped exceptions)))

(define (missing-closer-rate label files samples)
  (let ([texts (list->vector (map (lambda (p) (make-text (read-source-file p))) files))]
        [hits 0] [hinted 0] [tried 0])
    (do ([k 0 (fx+ k 1)]) ((fx= k samples))
      (let* ([t (vector-ref texts (random (vector-length texts)))]
             [c (read-cst t (make-reader-mode 'chezscheme 'no-fold))]
             [closers (filter (lambda (tok) (eq? (token-kind tok) 'close)) (vector->list (cst-tokens c)))])
        (when (and (pair? closers) (fx= (vector-length (cst-diagnostics c)) 0))
          (let* ([closer (random-element closers)]
                 [s (text-string t)]
                 [mutant (make-text (string-append (substring s 0 (token-start closer))
                                                   (substring s (token-end closer) (string-length s))))]
                 [line (text-offset-line t (token-start closer))]
                 [hints (filter (lambda (d) (eq? (diagnostic-rule d) 'missing-closer))
                                (vector->list (cst-diagnostics (read-cst mutant (make-reader-mode 'chezscheme 'no-fold)))))])
            (set! tried (fx+ tried 1))
            (unless (null? hints) (set! hinted (fx+ hinted 1)))
            (when (exists (lambda (d) (fx<= (abs (fx- (text-offset-line mutant (diagnostic-start d)) line)) 1)) hints)
              (set! hits (fx+ hits 1)))))))
    (printf "missing closer, ~a: ~a deletions, a hint in ~a, within one line in ~a (~,1f%)\n"
      label tried hinted hits (if (fx= tried 0) 0.0 (* 100.0 (/ hits tried))))))

(let ([dirs (corpus-directories)])
  (if (null? dirs)
      (printf "reader fuzzing: skipped, CHEZ_TOOLS_CORPORA is not set\n")
      (let ([files (apply append (map corpus-files dirs))])
        (run-mutants files)
        (for-each
          (lambda (dir)
            (let ([s-files (filter (lambda (p) (equal? (path-extension p) "ss"))
                             (if (file-exists? (string-append dir "/s/read.ss")) (corpus-files (string-append dir "/s")) '()))])
              (unless (null? s-files)
                (missing-closer-rate (string-append dir "/s") s-files 2000)))
            (missing-closer-rate dir (corpus-files dir) 2000))
          dirs)
        (printf "reader fuzzing: ~a failures\n" failures)
        (exit (if (fx= failures 0) 0 1)))))
