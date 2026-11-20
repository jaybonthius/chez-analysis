(import (chezscheme) (check) (analysis path))

(test "file URIs decode to paths"
  (check-equal? "/home/a b/\x3BB;.ss" (uri->path "file:///home/a%20b/%CE%BB.ss"))
  (check-equal? "/x/y" (uri->path "FILE:///x/y"))
  (check-equal? "/x/y" (uri->path "file://localhost/x/y"))
  (check-equal? "/x/y" (uri->path "file:/x/y"))
  (check-equal? "/x/a#b" (uri->path "file:///x/a%23b#fragment"))
  (check-equal? "/x/C:" (uri->path "file:///x/C%3a")))

(test "other URIs give #f"
  (for-each
    (lambda (uri) (check-equal? (list uri #f) (list uri (uri->path uri))))
    '("untitled:Untitled-1" "http://x/y" "file://host/x" "file:relative" "file:///x%2" "file:///x%zz"
      "file:///x%00y" "file:///x%FFy" "file:///\x3BB;")))

(test "paths encode one fixed way and round-trip"
  (check-equal? "file:///home/a%20b/%CE%BB-x_y.~z%25" (path->uri "/home/a b/\x3BB;-x_y.~z%"))
  (for-each
    (lambda (p) (check-equal? p (uri->path (path->uri p))))
    '("/" "/a/b.sls" "/with space/and#hash?q" "/\x1F600;/\x3BB;" "/a%41"))
  (check-raises assertion-violation? (path->uri "relative/x")))

(test "canonical paths resolve links and normalise absent files"
  (let ([dir (format "/tmp/chez-tools-path-test-~a" (random 1000000000))])
    (mkdir dir)
    (mkdir (string-append dir "/real"))
    (call-with-output-file (string-append dir "/real/f.ss") (lambda (p) (display "x" p)))
    (system (format "ln -s real ~a/link" dir))
    (check-equal? (string-append dir "/real/f.ss") (canonical-path (string-append dir "/link/f.ss")))
    (check-equal? (string-append dir "/real/absent.ss")
                  (canonical-path (string-append dir "/real/./x/../absent.ss")))
    (check-equal? "/" (canonical-path "/../.."))
    (system (format "rm -r ~a" dir))))

(test "paths relative to a directory"
  (check-equal? "b/c.ss" (path-relative-to "/a/b/c.ss" "/a"))
  (check-equal? "." (path-relative-to "/a/b" "/a/b/"))
  (check-equal? #f (path-relative-to "/x/b" "/a"))
  (check-equal? "c" (path-relative-to "/a/./b/../c" "/a")))

(test "globs match relative to their base"
  (let ([g (lambda (p) (compile-glob p "/r"))])
    (check (glob-match? (g "build/**") "/r/build/obj/x.so"))
    (check (glob-match? (g "build/**") "/r/build"))
    (check (not (glob-match? (g "build/**") "/r/src/build")))
    (check (glob-match? (g "**/*.ss") "/r/a.ss"))
    (check (glob-match? (g "**/*.ss") "/r/x/y/a.ss"))
    (check (not (glob-match? (g "*.ss") "/r/x/a.ss")))
    (check (glob-match? (g "tests/?-test.sps") "/r/tests/a-test.sps"))
    (check (not (glob-match? (g "tests/?-test.sps") "/r/tests/ab-test.sps")))
    (check (not (glob-match? (g "*.ss") "/other/a.ss")))
    (check-equal? "*.ss" (glob-pattern (g "*.ss")))))

(test "a star or a ** that first matched too little matches more when the rest of the pattern needs it"
  (let ([m? (lambda (p path) (glob-match? (compile-glob p "/r") (string-append "/r/" path)))])
    (check (m? "*a?c" "aXaYc"))
    (check (not (m? "a*?" "a")))
    (check (m? "a*?" "ab"))
    (check (m? "*ab*ab" "xabyabab"))
    (check (m? "**/x/**/y" "x/a/x/b/y"))
    (check (m? "**/b/c" "b/c/b/c"))
    (check (not (m? "**/b/c" "b/c/b")))
    (check (m? "a/**/b" "a/b"))
    (check (not (m? "a/**/b" "ab")))
    (check (not (m? "**/foo.ss" "dir/barfoo.ss")))
    (check (m? "**/*-test.sps" "t/u/x-test.sps"))))

(define (seconds-taken thunk)
  (let ([start (current-time 'time-monotonic)])
    (thunk)
    (let ([d (time-difference (current-time 'time-monotonic) start)])
      (+ (time-second d) (/ (time-nanosecond d) 1e9)))))

(test "matching takes time in proportion to the input, however many stars the pattern has"
  (let ([stars (apply string-append (make-list 14 "*a"))]
        [as (make-string 60 #\a)]
        [dirs (apply string-append (make-list 60 "a/"))])
    (check (< (seconds-taken
                (lambda ()
                  (check (not (glob-match? (compile-glob (string-append stars "*b") "/r") (string-append "/r/" as))))
                  (check (glob-match? (compile-glob (string-append stars "*") "/r") (string-append "/r/" as)))
                  (check (not (glob-match? (compile-glob (string-append (apply string-append (make-list 14 "**/a/")) "b") "/r")
                                           (string-append "/r/" dirs "c"))))
                  (check (glob-match? (compile-glob (string-append (apply string-append (make-list 14 "**/a/")) "**") "/r")
                                      (string-append "/r/" dirs "c")))
                  (check (not (wildcard-match? (string-append stars "*b") as)))
                  (check (wildcard-match? (string-append stars "*") as))))
              1))))

(test "a wildcard pattern's star matches any characters, and every other character only itself"
  (check (wildcard-match? "_*" "_x"))
  (check (wildcard-match? "_*" "_"))
  (check (not (wildcard-match? "_*" "x_")))
  (check (wildcard-match? "*-tmp" "a-b-tmp"))
  (check (wildcard-match? "a*b*c" "aXbYbc"))
  (check (not (wildcard-match? "a*b*c" "aXbYbd")))
  (check (wildcard-match? "ok?" "ok?"))
  (check (not (wildcard-match? "ok?" "okx")))
  (check (wildcard-match? "" ""))
  (check (not (wildcard-match? "" "x")))
  (check (wildcard-match? "**" "")))

(test-exit)
