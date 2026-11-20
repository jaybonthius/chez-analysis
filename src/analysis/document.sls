(library (analysis document)
  (export read-document file-kind-of file-kind-guessed?)
  (import (chezscheme) (analysis reader) (analysis config) (analysis text))

  (define (first-datum-head c)
    (let ([forms (cst-top-level-forms c)])
      (and (fx> (vector-length forms) 0)
           (let ([x (top-level-form-node (vector-ref forms 0))])
             (and (node? x)
                  (let ([head (node-head-token x)])
                    (and head (symbol-token-name c head))))))))

  ;; A file is read as R6RS when its extension or its first datum says so,
  ;; as Chez reads it when it loads the file as a program or a library, and
  ;; with the project's case folding until a directive changes it.
  (define (read-document config path t)
    (unless (config? config) (assertion-violationf 'read-document "~s is not a configuration" config))
    (unless (text? t) (assertion-violationf 'read-document "~s is not a text" t))
    (let* ([fold (if (config-case-sensitive? config) (fold-mode no-fold) (fold-mode fold))]
           [configured (and path (config-reader-mode config path))]
           [chez (make-reader-mode (syntax-mode chezscheme) fold)]
           [r6rs (make-reader-mode (syntax-mode r6rs) fold)])
      (cond
        [configured (read-cst t (if (eq? configured 'r6rs) r6rs chez))]
        [(and path (equal? (path-extension path) "sps")) (read-cst t r6rs)]
        [else
         (let ([c (read-cst t chez)])
           (if (member (first-datum-head c) '("library" "top-level-program"))
               (read-cst t r6rs)
               c))])))

  (define (script-header c)
    (let ([tokens (cst-tokens c)])
      (and (fx> (vector-length tokens) 0)
           (eq? (token-kind (vector-ref tokens 0)) 'script-header)
           (token-text c (vector-ref tokens 0)))))

  (define (header-words s)
    (let loop ([i 0] [start #f] [acc '()])
      (cond
        [(fx= i (string-length s)) (reverse (if start (cons (substring s start i) acc) acc))]
        [(char-whitespace? (string-ref s i)) (loop (fx+ i 1) #f (if start (cons (substring s start i) acc) acc))]
        [else (loop (fx+ i 1) (or start i) acc)])))

  ;; scheme-script is Chez's name for scheme --program.
  (define (runs-as-program? header)
    (exists (lambda (word) (or (string=? word "--program") (string=? (path-last word) "scheme-script")))
            (header-words header)))

  ;; Chez runs a file headed by (import ...) as a script or as a program by
  ;; its command line, not its text. The guess is script: a wrong guess
  ;; toward program raises false errors, such as an unbound define, that
  ;; end their form and hide what follows, while one toward script only
  ;; misses findings.
  (define (file-kind-and-guess config path c includers)
    (cond
      [(and path (config-file-kind config path)) => (lambda (kind) (values kind #f))]
      [(pair? includers) (values (file-kind include) #f)]
      [else
       (let ([head (first-datum-head c)] [header (script-header c)])
         (cond
           [(equal? head "library") (values (file-kind library) #f)]
           [(or (and path (equal? (path-extension path) "sps")) (equal? head "top-level-program")
                (and header (runs-as-program? header)))
            (values (file-kind program) #f)]
           [else (values (file-kind script) (and (equal? head "import") (not header)))]))]))

  (define (file-kind-of config path c includers)
    (let-values ([(kind guessed?) (file-kind-and-guess config path c includers)])
      kind))

  (define (file-kind-guessed? config path c includers)
    (let-values ([(kind guessed?) (file-kind-and-guess config path c includers)])
      guessed?)))
