(import (chezscheme) (check) (analysis config) (analysis posix) (analysis path))

(define root
  (let ([dir (format "/tmp/chez-tools-config-test-~a" (process-id))])
    (system (format "rm -rf ~a; mkdir -p ~a/a/b/c" dir dir))
    dir))

(define (write-config! dir text)
  (call-with-output-file (string-append dir "/.chez-tools")
    (lambda (out) (put-string out text))
    'truncate))

(define (remove-config! dir) (delete-file (string-append dir "/.chez-tools")))

(define (config-error-at text)
  (write-config! (string-append root "/a") text)
  (guard (c [(config-error? c) (list (config-error-start c) (config-error-end c) (condition-message c))])
    (find-project-config (string-append root "/a/x.ss") root)
    'no-error))

(test "without a file every entry has its default and the root is the fallback"
  (let ([c (find-project-config (string-append root "/a/b/x.ss") (string-append root "/a"))])
    (check-equal? (string-append root "/a") (config-root c))
    (check-equal? 'fallback (config-root-marker c))
    (check-equal? #f (config-library-directories c))
    (check-equal? '(".chezscheme.sls" ".ss" ".sls" ".scm" ".sch") (config-library-extensions c))
    (check-equal? '(".sls" ".sps" ".ss" ".scm") (config-source-extensions c))
    (check-equal? #t (config-case-sensitive? c))
    (check-equal? '() (config-preload c))
    (check-equal? '() (config-files c))
    (check (member (string-append root "/a/b/.chez-tools") (config-absent-files c)))
    (check (member "/.chez-tools" (config-absent-files c)))
    (let ([w (config-worker-limits c)])
      (check-equal? (* 4096 1024 1024) (worker-limits-memory w))
      (check-equal? (quotient (memory-budget-bytes) 2) (worker-limits-memory-budget w))
      (check-equal? '(60 100 3) (list (worker-limits-timeout w) (worker-limits-analyses w) (worker-limits-foreground w))))
    (let ([l (config-lint c)])
      (check-equal? '() (lint-settings-switches l))
      (check-equal? '("_" "_*" "ignore") (lint-settings-unused-names l))
      (check-equal? #f (lint-settings-undefined-variable-warnings? l)))))

(test "every entry decodes"
  (putenv "CHEZ_TOOLS_CONFIG_TEST" "/x/lib::/x/obj:rel")
  (write-config! (string-append root "/a")
    (string-append
      "(root)\n"
      "(library-directories \"src\" \"/abs\" (environment-variable \"CHEZ_TOOLS_CONFIG_TEST\"))\n"
      "(library-extensions \".sls\" \".ss\")\n"
      "(source-extensions \".sls\" \".sps\")\n"
      "(exclude \"build/**\")\n"
      "(file-kind \"tests/**/*.ss\" program)\n"
      "(file-kind \"**/*.ss\" script)\n"
      "(reader-mode \"compat/**/*.ss\" chezscheme)\n"
      "(case-sensitive #f)\n"
      "(preload \"build/prelude.so\")\n"
      "(definition-forms define define-my-thing)\n"
      "(syntax-binding-forms define-syntax)\n"
      "(worker (memory-limit 512) (memory-budget 2048) (timeout 5) (analyses 7) (foreground 2))\n"
      "(lint (enable unused-parameter) (disable argument-count) (severity format-string error)\n"
      "      (ignore \"tests/**\" compiler-warning) (unused-names \"_\") (undefined-variable-warnings #t))\n"
      "(format (call-layout author) (head-alone 2) (comment-lines with-code)\n"
      "        (column-zero-heads library) (indent (my-macro 1) ((with-foo with-bar) 1) (0 define-my-table) (other #f)))\n"))
  (let* ([a (string-append root "/a")] [c (find-project-config (string-append a "/b/x.ss") "/")])
    (check-equal? a (config-root c))
    (check-equal? (list (string-append a "/src") "/abs" "/x/lib" (string-append a "/rel")) (config-library-directories c))
    (check-equal? '(("CHEZ_TOOLS_CONFIG_TEST" . "/x/lib::/x/obj:rel")) (config-environment c))
    (check-equal? '(".sls" ".ss") (config-library-extensions c))
    (check-equal? '(".sls" ".sps") (config-source-extensions c))
    (check (config-excluded? c (string-append a "/build/obj/x.so")))
    (check (not (config-excluded? c (string-append a "/src/x.sls"))))
    (check-equal? 'program (config-file-kind c (string-append a "/tests/t/x.ss")))
    (check-equal? 'script (config-file-kind c (string-append a "/y.ss")))
    (check-equal? #f (config-file-kind c (string-append a "/y.sls")))
    (check-equal? 'chezscheme (config-reader-mode c (string-append a "/compat/x.ss")))
    (check-equal? #f (config-case-sensitive? c))
    (check-equal? (list (string-append a "/build/prelude.so")) (config-preload c))
    (check-equal? '("define" "define-my-thing") (config-definition-forms c))
    (check-equal? '("define-syntax") (config-syntax-binding-forms c))
    (let ([w (config-worker-limits c)])
      (check-equal? (list (* 512 1024 1024) (* 2048 1024 1024) 5 7 2)
        (list (worker-limits-memory w) (worker-limits-memory-budget w) (worker-limits-timeout w)
              (worker-limits-analyses w) (worker-limits-foreground w))))
    (let ([l (config-lint c)])
      (check-equal? '((unused-parameter . #t) (argument-count . #f)) (lint-settings-switches l))
      (check-equal? '((format-string . error)) (lint-settings-severities l))
      (check-equal? '((compiler-warning)) (map lint-ignore-rules (lint-settings-ignores l)))
      (check (glob-match? (lint-ignore-glob (car (lint-settings-ignores l))) (string-append a "/tests/x.ss")))
      (check-equal? '("_") (lint-settings-unused-names l))
      (check-equal? #t (lint-settings-undefined-variable-warnings? l))
      (check-equal? '(unused-parameter argument-count format-string compiler-warning)
        (map rule-mention-name (lint-settings-mentions l))))
    (check-equal? '(("my-macro") . 1) (car (cdr (assq 'indent (config-format c)))))
    (check-equal? '(("with-foo" "with-bar") . 1) (cadr (cdr (assq 'indent (config-format c)))))
    (check-equal? '(("define-my-table") . 0) (caddr (cdr (assq 'indent (config-format c)))))
    (check-equal? 'author (cdr (assq 'call-layout (config-format c)))))
  (remove-config! (string-append root "/a")))

(test "nearer files win per entry, and (root) stops the search"
  (write-config! root "(library-directories \"top\") (case-sensitive #f) (worker (timeout 9) (analyses 4))")
  (write-config! (string-append root "/a/b") "(worker (timeout 3))")
  (let ([c (find-project-config (string-append root "/a/b/c/x.ss") "/")])
    (check-equal? root (config-root c))
    (check-equal? 'configuration (config-root-marker c))
    (check-equal? (list (string-append root "/top")) (config-library-directories c))
    (check-equal? #f (config-case-sensitive? c))
    (check-equal? '(3 4) (list (worker-limits-timeout (config-worker-limits c))
                               (worker-limits-analyses (config-worker-limits c))))
    (check-equal? (list (string-append root "/a/b/.chez-tools") (string-append root "/.chez-tools"))
      (map car (config-files c)))
    (check (member (string-append root "/a/b/c/.chez-tools") (config-absent-files c)))
    (check (member (string-append root "/a/.chez-tools") (config-absent-files c))))
  (write-config! (string-append root "/a") "(root) (case-sensitive #t)")
  (let ([c (find-project-config (string-append root "/a/b/c/x.ss") "/")])
    (check-equal? (string-append root "/a") (config-root c))
    (check-equal? #t (config-case-sensitive? c))
    (check-equal? 2 (length (config-files c))))
  (for-each remove-config! (list root (string-append root "/a") (string-append root "/a/b"))))

(test "the library layout is set only in the project's root file"
  (write-config! root "(case-sensitive #f)")
  (write-config! (string-append root "/a") "(worker (timeout 3))\n(exclude \"x/**\")")
  (check-equal? (list 21 37 (format "exclude is accepted only in the project's root .chez-tools, ~a/.chez-tools" root))
    (guard (c [(config-error? c) (list (config-error-start c) (config-error-end c) (condition-message c))])
      (find-project-config (string-append root "/a/b/x.ss") "/")
      'no-error))
  (for-each remove-config! (list root (string-append root "/a"))))

(define (git-repository! dir) (mkdir (string-append dir "/.git")))

(define (write-file! path text)
  (call-with-output-file path (lambda (out) (put-string out text)) 'truncate))

(define (fresh-tree name)
  (let ([dir (string-append root "/" name)])
    (system (format "rm -rf ~a; mkdir -p ~a/sub/inner ~a/sub/nested" dir dir dir))
    dir))

(test "a repository ends the search and is the project when no file names one"
  (let ([outer (fresh-tree "git")])
    (git-repository! outer)
    (write-config! root "(case-sensitive #f)")
    (let ([c (find-project-config (string-append outer "/sub/x.ss") "/")])
      (check-equal? outer (config-root c))
      (check-equal? 'git (config-root-marker c))
      (check-equal? #t (config-case-sensitive? c)))
    (remove-config! root)))

(test "a submodule belongs to the enclosing project only when that project has a root .chez-tools"
  (let ([outer (fresh-tree "submodule")])
    (git-repository! outer)
    (write-file! (string-append outer "/sub/.git") "gitdir: ../.git/modules/sub\n")
    (write-file! (string-append outer "/.gitmodules") "[submodule \"sub\"]\n\tpath = sub\n\turl = ../sub.git\n")
    (let ([c (find-project-config (string-append outer "/sub/x.ss") "/")])
      (check-equal? (string-append outer "/sub") (config-root c))
      (check-equal? 'git (config-root-marker c))
      (check (member (string-append outer "/.chez-tools") (config-absent-files c))))
    (write-config! outer "")
    (write-config! (string-append outer "/sub") "(library-directories \"lib\") (case-sensitive #f)")
    (let ([c (find-project-config (string-append outer "/sub/inner/x.ss") "/")])
      (check-equal? outer (config-root c))
      (check-equal? 'configuration (config-root-marker c))
      (check-equal? #f (config-library-directories c))
      (check-equal? #f (config-case-sensitive? c))
      (check-equal? (list (format "~a/sub/.chez-tools: library-directories is ignored, because this file belongs to a submodule of the project at ~a, whose own .chez-tools sets the library layout" outer outer))
        (config-notes c))
      (check (assoc (string-append outer "/.gitmodules") (config-files c))))
    (write-config! (string-append outer "/sub") "(root)")
    (check-equal? (string-append outer "/sub") (config-root (find-project-config (string-append outer "/sub/x.ss") "/")))
    (write-config! (string-append outer "/sub") "")
    (write-config! outer "(exclude \"sub/**\")")
    (let ([c (find-project-config (string-append outer "/sub/inner/x.ss") "/")])
      (check-equal? (string-append outer "/sub") (config-root c))
      (check-equal? 'configuration (config-root-marker c)))))

(test "a nested repository that no .gitmodules lists, such as a worktree, is a project of its own"
  (let ([outer (fresh-tree "worktree")])
    (git-repository! outer)
    (write-config! outer "")
    (write-file! (string-append outer "/sub/.git") "gitdir: ../.git/worktrees/sub\n")
    (let ([c (find-project-config (string-append outer "/sub/x.ss") "/")])
      (check-equal? (string-append outer "/sub") (config-root c))
      (check-equal? 'git (config-root-marker c)))))

(test "membership holds through submodules of submodules"
  (let ([outer (fresh-tree "recursive")])
    (git-repository! outer)
    (write-config! outer "")
    (write-file! (string-append outer "/.gitmodules") "[submodule \"sub\"]\n\tpath = sub\n")
    (write-file! (string-append outer "/sub/.git") "gitdir: ../.git/modules/sub\n")
    (write-file! (string-append outer "/sub/.gitmodules") "[submodule \"nested\"]\n\tpath = \"nested\"\n")
    (write-file! (string-append outer "/sub/nested/.git") "gitdir: ../../.git/modules/sub/modules/nested\n")
    (check-equal? outer (config-root (find-project-config (string-append outer "/sub/nested/x.ss") "/")))
    (delete-file (string-append outer "/sub/.git"))
    (mkdir (string-append outer "/sub/.git"))
    (check-equal? outer (config-root (find-project-config (string-append outer "/sub/nested/x.ss") "/")))
    (delete-file (string-append outer "/.chez-tools"))
    (check-equal? (string-append outer "/sub/nested")
      (config-root (find-project-config (string-append outer "/sub/nested/x.ss") "/")))))

(test "a file read alone has its directory as its root"
  (write-config! (string-append root "/a/b") "(library-directories \"lib\")")
  (let ([c (read-config-file (string-append root "/a/b/.chez-tools"))])
    (check-equal? (string-append root "/a/b") (config-root c))
    (check-equal? (list (string-append root "/a/b/lib")) (config-library-directories c)))
  (remove-config! (string-append root "/a/b")))

(test "a string in the file means what it means to Chez"
  (write-config! (string-append root "/a") "(lint (unused-names \"a\\\n   b\" \"\\x5F;*\" \"\\v\\f\\101\"))")
  (check-equal? '("ab" "_*" "\v\fA")
    (lint-settings-unused-names (config-lint (find-project-config (string-append root "/a/x.ss") root))))
  (remove-config! (string-append root "/a")))

(test "malformed files report the position of the problem"
  (check-equal? '(0 8 "unknown entry exit") (config-error-at "(exit 1)"))
  (check-equal? '(8 24 "memory-limit takes one positive integer") (config-error-at "(worker (memory-limit 0))"))
  (check-equal? '(6 20 "unknown lint entry enable-all") (config-error-at "(lint (enable-all x))"))
  (check-equal? '(21 30) (list-head (config-error-at "(library-directories (car \"x\"))") 2))
  (check-equal? '(0 15 "unexpected end-of-file reading list") (config-error-at "(case-sensitive"))
  (check-equal? '(9 13) (list-head (config-error-at "(exclude #(1))") 2))
  (check-equal? "case-sensitive appears twice in this file"
    (caddr (config-error-at "(case-sensitive #t) (case-sensitive #f)")))
  (check-equal? 'no-error (config-error-at "(preload \"a\\x41;\\\\b\") ; comment\n#| block |#"))
  (remove-config! (string-append root "/a")))

(test "nothing in the file is evaluated"
  (write-config! (string-append root "/a") "(exit 1)")
  (check-raises config-error? (find-project-config (string-append root "/a/x.ss") "/"))
  (write-config! (string-append root "/a") "(library-directories #.(exit 1))")
  (check-raises config-error? (find-project-config (string-append root "/a/x.ss") "/"))
  (remove-config! (string-append root "/a")))

(system (format "rm -rf ~a" root))

(test-exit)
