(library (fixtures)
  (export make-project project-root project-file write-project-file! remove-project!)
  (import (chezscheme) (analysis posix))

  (define-record-type (project new-project project?)
    (nongenerative) (sealed #t)
    (fields root))

  (define (write-text! file text)
    (let ([dir (path-parent file)])
      (system (format "mkdir -p '~a'" dir)))
    (call-with-output-file file (lambda (out) (put-string out text)) 'truncate))

  (define (make-project name entries)
    (let ([root (format "/tmp/cz-~a-~a" name (process-id))])
      (system (format "rm -rf '~a'" root))
      (for-each (lambda (e) (write-text! (string-append root "/" (car e)) (cdr e))) entries)
      (new-project root)))

  (define (project-file p name) (string-append (project-root p) "/" name))

  (define (write-project-file! p name text) (write-text! (project-file p name) text))

  (define (remove-project! p) (system (format "rm -rf '~a'" (project-root p)))))
