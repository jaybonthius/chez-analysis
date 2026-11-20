;;; Warnings fail the build: Chez reports wrong argument counts only as
;;; warnings.

(unless (and (equal? (call-with-values scheme-version-number list) '(10 4 1))
             (threaded?))
  (errorf 'build "need threaded Chez Scheme 10.4.1, not ~a (~a)"
    (scheme-version) (if (threaded?) "threaded" "non-threaded")))

(define object-root "build/obj")
(define library-roots '("src" "tests/lib"))

;; include searches source-directories, never the including file's directory;
;; "." lets the test library locate the test programs' own source.
(source-directories '("src" "."))
(library-directories
  (map (lambda (root) (cons root (string-append object-root "/" root)))
       library-roots))
(compile-imported-libraries #t)
;; A program built on these libraries can link them into one file only
;; from their .wpo files.
(generate-wpo-files #t)
(optimize-level 2)
(generate-inspector-information #t)
(compile-file-message #f)

(define compiled 0)
(define problems 0)

(define (report c)
  (set! problems (+ problems 1))
  (display-condition c (console-error-port))
  (newline (console-error-port)))

;; Compiling a library compiles its body twice, once to install it and once
;; for the object file, so one warning can be raised twice; report each
;; distinct message once.
(define seen-warnings (make-hashtable string-hash string=?))

;; An object compiled with a warning is still written; deleting it makes the
;; next build compile it again and fail again until the source is fixed.
(define (compiling src obj compile)
  (set! compiled (+ compiled 1))
  (printf "compiling ~a\n" src)
  (let ([problems-before problems] [finished? #f])
    (dynamic-wind
      void
      (lambda () (compile src obj) (set! finished? #t))
      (lambda ()
        (unless (and finished? (= problems problems-before))
          (when (file-exists? obj) (delete-file obj)))))))

(compile-library-handler (lambda (src obj) (compiling src obj compile-library)))
(compile-program-handler (lambda (src obj) (compiling src obj compile-program)))

;; A guard would stop the compile at the first warning; returning from the
;; handler lets it continue and report every warning.
(define (with-warnings-as-errors thunk)
  (guard (c [#t (report c)])
    (with-exception-handler
      (lambda (c)
        (if (warning? c)
            (let ([text (call-with-string-output-port
                          (lambda (p) (display-condition c p)))])
              (unless (hashtable-ref seen-warnings text #f)
                (hashtable-set! seen-warnings text #t)
                (report c)))
            (raise-continuable c)))
      thunk)))

(define (ensure-parent-directory file)
  (let loop ([d (path-parent file)])
    (unless (or (string=? d "") (file-directory? d))
      (loop (path-parent d))
      (mkdir d))))

(define (find-files dir extension)
  (if (file-directory? dir)
      (let walk ([dir dir])
        (apply append
          (map (lambda (f)
                 (let ([p (string-append dir "/" f)])
                   (cond
                     [(file-directory? p) (walk p)]
                     [(equal? (path-extension p) extension) (list p)]
                     [else '()])))
               (sort string<? (directory-list dir)))))
      '()))

(define (object-file src)
  (string-append object-root "/" (path-root src) ".so"))

;; An object without its .wpo file cannot be linked into a program, so it is
;; compiled again.
(define (build-each maybe-compile sources)
  (for-each
    (lambda (src)
      (let ([obj (object-file src)])
        (ensure-parent-directory obj)
        (when (and (file-exists? obj) (not (file-exists? (string-append (path-root obj) ".wpo"))))
          (delete-file obj))
        (with-warnings-as-errors (lambda () (maybe-compile src obj)))))
    sources))

(build-each maybe-compile-library
  (apply append (map (lambda (root) (find-files root "sls")) library-roots)))

(build-each maybe-compile-program
  (filter (lambda (f) (string=? (path-parent f) "tests"))
          (find-files "tests" "sps")))

(printf "build: ~a compiled, ~a problems\n" compiled problems)
(exit (if (= problems 0) 0 1))
