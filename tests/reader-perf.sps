(import (chezscheme) (reader-oracle) (analysis text) (analysis reader))

(define (s-directory)
  (find (lambda (dir) (file-directory? (string-append dir "/s")))
        (filter (lambda (dir) (file-exists? (string-append dir "/s/read.ss"))) (corpus-directories))))

(define (milliseconds thunk)
  (collect (collect-maximum-generation))
  (let ([start (current-time 'time-monotonic)])
    (thunk)
    (let ([d (time-difference (current-time 'time-monotonic) start)])
      (+ (* 1000 (time-second d)) (/ (time-nanosecond d) 1e6)))))

(define (best-of n thunk)
  (let loop ([i 0] [best +inf.0])
    (if (fx= i n) best (loop (fx+ i 1) (min best (milliseconds thunk))))))

(let ([dir (s-directory)])
  (if (not dir)
      (begin
        (printf "reader performance: skipped, no corpus in CHEZ_TOOLS_CORPORA holds Chez's s/read.ss\n")
        (exit 0))
      (let* ([files (filter (lambda (p) (equal? (path-extension p) "ss"))
                            (corpus-files (string-append dir "/s")))]
             [texts (map (lambda (p) (make-text (read-source-file p))) files)]
             [characters (apply + (map text-length texts))]
             [mode (make-reader-mode 'chezscheme 'no-fold)]
             [ours (best-of 5 (lambda () (for-each (lambda (t) (read-cst t mode)) texts)))]
             [chez (best-of 5
                     (lambda ()
                       (for-each
                         (lambda (t)
                           (let ([ip (open-string-input-port (text-string t))]
                                 [sfd (make-source-file-descriptor "s" (open-bytevector-input-port #vu8()))])
                             (let loop ([fp 0])
                               (let-values ([(x end) (get-datum/annotations ip sfd fp)])
                                 (unless (eof-object? x) (loop end))))))
                         texts)))])
        (printf "reader performance on ~a files of s/, ~a characters: read-cst ~,1f ms, get-datum/annotations ~,1f ms (best of 5)\n"
          (length files) characters ours chez)
        (exit (if (<= ours chez) 0 1)))))
