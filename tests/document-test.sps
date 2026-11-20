(import (chezscheme) (check) (fixtures) (analysis config) (analysis document) (analysis text))

(test "a file headed by import is a guessed script unless its configuration, its extension or a #! line gives its kind"
  (let* ([p (make-project "file-kinds" (list (cons ".chez-tools" "(root)\n(file-kind \"configured*.ss\" program)\n(file-kind \"given-script.ss\" script)\n")))]
         [config (find-project-config (project-file p "x.ss") (project-root p))]
         [kind (lambda (name text)
                 (let* ([path (project-file p name)] [c (read-document config path (make-text text))])
                   (list (file-kind-of config path c '()) (file-kind-guessed? config path c '()))))]
         [body "(import (lib a))\n(define (show) (x))\n"])
    (check-equal? '(script #t) (kind "guessed.ss" body))
    (check-equal? '(script #f) (kind "given-script.ss" body))
    (check-equal? '(program #f) (kind "configured.ss" body))
    (check-equal? '(program #f) (kind "main.sps" body))
    (check-equal? '(program #f) (kind "env.ss" (string-append "#! /usr/bin/env scheme-script\n" body)))
    (check-equal? '(program #f) (kind "path.ss" (string-append "#!/usr/local/bin/scheme-script\n" body)))
    (check-equal? '(program #f) (kind "option.ss" (string-append "#!/usr/bin/scheme --program\n" body)))
    (check-equal? '(script #f) (kind "script-option.ss" (string-append "#!/usr/bin/scheme --script\n" body)))
    (check-equal? '(script #t) (kind "directive.ss" (string-append "#!r6rs\n" body)))
    (check-equal? '(program #f) (kind "wrapped.ss" "(top-level-program (import (chezscheme)) (display 1))\n"))
    (check-equal? '(library #f) (kind "lib.ss" "(library (lib) (export) (import (chezscheme)))\n"))
    (check-equal? '(script #f) (kind "plain.ss" "(define (show) 1)\n"))
    (let* ([path (project-file p "included.ss")] [c (read-document config path (make-text body))])
      (check-equal? '(include #f) (list (file-kind-of config path c (list (project-file p "main.sps")))
                                        (file-kind-guessed? config path c (list (project-file p "main.sps"))))))
    (remove-project! p)))

(test-exit)
