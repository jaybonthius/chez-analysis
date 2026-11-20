(import (chezscheme) (reader-oracle))

(define failures 0)
(define shown 0)
(define max-shown 40)

(define (report-failures! label comparison)
  (for-each
    (lambda (f)
      (set! failures (fx+ failures 1))
      (when (fx< shown max-shown)
        (set! shown (fx+ shown 1))
        (printf "FAIL ~a [~a]: ~a\n" label (car f) (cdr f))))
    (comparison-failures comparison)))

(define-record-type tally
  (nongenerative) (sealed #t)
  (fields name (mutable inputs) (mutable read) (mutable errors) (mutable exact) (mutable skipped)))

(define (new-tally name) (make-tally name 0 0 0 0 0))

(define (count! t comparison)
  (tally-inputs-set! t (fx+ (tally-inputs t) 1))
  (cond
    [(comparison-chez-read? comparison) (tally-read-set! t (fx+ (tally-read t) 1))]
    [(comparison-skipped comparison) (tally-skipped-set! t (fx+ (tally-skipped t) 1))]
    [else
     (tally-errors-set! t (fx+ (tally-errors t) 1))
     (when (comparison-exact-message? comparison) (tally-exact-set! t (fx+ (tally-exact t) 1)))]))

(define (print-tally t)
  (printf "~a: ~a inputs, ~a read by Chez, ~a rejected (first error text identical in ~a), ~a skipped as process-dependent\n"
    (tally-name t) (tally-inputs t) (tally-read t) (tally-errors t) (tally-exact t) (tally-skipped t)))

(define syntax-modes '((chezscheme no-fold) (r6rs no-fold)))
(define all-modes '((chezscheme no-fold) (r6rs no-fold) (chezscheme fold) (r6rs fold)))

(define (compare! t label s modes)
  (for-each
    (lambda (mode)
      (let ([comparison (compare-with-chez s (car mode) (cadr mode))])
        (count! t comparison)
        (report-failures! (format "~a ~a" label mode) comparison)))
    modes))

(define (run-corpora)
  (let ([dirs (corpus-directories)])
    (when (null? dirs)
      (printf "corpora: skipped, CHEZ_TOOLS_CORPORA is not set\n"))
    (for-each
      (lambda (dir)
        (let ([t (new-tally dir)])
          (for-each (lambda (path) (compare! t path (read-source-file path) all-modes)) (corpus-files dir))
          (print-tally t)))
      dirs)))

(define (xmat-strings path)
  (call-with-port (open-input-file path)
    (lambda (ip)
      (let loop ([acc '()])
        (let ([x (read ip)])
          (cond
            [(eof-object? x) (reverse acc)]
            [(and (pair? x) (eq? (car x) 'xmat))
             (loop (append (reverse (filter string? (cdr x))) acc))]
            [else (loop acc)]))))))

(define (run-reader-error-cases)
  (let ([paths (filter (lambda (p) (let ([n (string-length p)])
                                      (and (fx>= n 9) (string=? (substring p (fx- n 9) n) "mats/6.ms"))))
                 (apply append (map corpus-files (corpus-directories))))])
    (if (null? paths)
        (printf "mats/6.ms error cases: skipped, no corpus holds mats/6.ms\n")
        (let ([t (new-tally "mats/6.ms error cases")])
          (for-each
            (lambda (s) (compare! t (format "~s" s) s all-modes))
            (xmat-strings (car paths)))
          (print-tally t)))))

(define alphabet (string->list "()[]{}\";#|\\'`,@.+-0178aeixrvuft!=%: \n"))

(define (run-short-strings)
  (let ([t (new-tally (format "strings of length 0 to 4 over ~a characters" (length alphabet)))]
        [chars (list->vector alphabet)])
    (let loop ([prefix ""] [depth 0])
      (compare! t (format "~s" prefix) prefix syntax-modes)
      (when (fx< depth 4)
        (vector-for-each
          (lambda (c) (loop (string-append prefix (string c)) (fx+ depth 1)))
          chars)))
    (print-tally t)))

(define (run-code-points)
  (let ([t (new-tally "every code point alone, as a character, in a string and after a symbol")])
    (do ([i 0 (fx+ i 1)]) ((fx> i #x10FFFF))
      (unless (fx<= #xD800 i #xDFFF)
        (let ([c (string (integer->char i))])
          (for-each (lambda (s) (compare! t (format "~s" s) s all-modes))
            (list c (string-append "#\\" c) (string-append "\"" c "\"") (string-append "(a" c ")"))))))
    (print-tally t)))

(run-corpora)
(run-reader-error-cases)
(run-short-strings)
(run-code-points)
(printf "reader differential: ~a failures\n" failures)
(exit (if (fx= failures 0) 0 1))
