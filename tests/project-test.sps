(import (chezscheme) (check) (fixtures) (analysis config) (analysis project))

(define (library-text name) (format "(library ~a (export) (import (chezscheme)))\n" name))

(define (layout-of p)
  (compute-project-layout (find-project-config (project-file p "x.ss") (project-root p)) #f))

(define (under p . names) (map (lambda (name) (project-file p name)) names))

(define (ambiguous-files layout)
  (map (lambda (entry) (cons (car entry) (map library-file-path (cdr entry)))) (project-layout-ambiguous layout)))

(define (contains? needle haystack)
  (let ([n (string-length needle)])
    (let loop ([i 0])
      (and (fx<= (fx+ i n) (string-length haystack))
           (or (string=? needle (substring haystack i (fx+ i n))) (loop (fx+ i 1)))))))

(define (note-for layout name)
  (let-values ([(note file) (unresolved-import-note layout name)]) note))

(test "one root is inferred from the library headers of a project"
  (let ([p (make-project "layout-single"
             (list (cons "src/a/b.sls" (library-text "(a b)"))
                   (cons "src/c.sls" (library-text "(c (1))"))
                   (cons "main.sps" "(import (chezscheme) (a b) (c))\n")))])
    (let ([layout (layout-of p)])
      (check-equal? 'inferred (project-layout-source layout))
      (check-equal? (under p "src") (project-layout-library-directories layout))
      (check-equal? '() (project-layout-ambiguous layout))
      (check-equal? '(("a" "b") ("c")) (map library-file-name (project-layout-library-files layout))))
    (remove-project! p)))

(test "several roots are listed in the order of their paths"
  (let ([p (make-project "layout-several"
             (list (cons "src/s.sls" (library-text "(s)"))
                   (cons "tests/lib/t.sls" (library-text "(t)"))
                   (cons "examples/x/e/f.ss" (library-text "(e f)"))))])
    (check-equal? (under p "examples/x" "src" "tests/lib") (project-layout-library-directories (layout-of p)))
    (remove-project! p)))

(test "a header that does not match its path gives no root, and the note at its import says why"
  (let ([p (make-project "layout-mismatch"
             (list (cons "src/x.sls" (library-text "(y)"))
                   (cons "src/a/b.sls" (library-text "(a b)"))
                   (cons "src/a/b.guile.sls" (library-text "(a b)"))))])
    (let ([layout (layout-of p)])
      (check-equal? (under p "src") (project-layout-library-directories layout))
      (check-equal? '() (project-layout-ambiguous layout))
      (check-equal? (format "(y) is defined in ~a, whose path does not end in y with a library extension, so Chez cannot find it by name"
                      (project-file p "src/x.sls"))
        (note-for layout '("y")))
      (check-equal? #f (note-for layout '("nowhere"))))
    (remove-project! p)))

(test "a root outside the project is not used, and the note names the directory Chez would need"
  (let* ([p (make-project "layout-outside" '())]
         [repository (path-last (project-root p))]
         [name (list repository "lib" "install")])
    (write-project-file! p "lib/install.sls" (library-text (format "(~a lib install)" repository)))
    (let ([layout (layout-of p)])
      (check-equal? '() (project-layout-library-directories layout))
      (check-equal? (format "(~a lib install) is defined in ~a, which Chez finds only with /tmp, outside the project, as a library directory; add it to library-directories or use Akku"
                      repository (project-file p "lib/install.sls"))
        (note-for layout name)))
    (remove-project! p)))

(test "Akku's .akku/lib is used when the project has one, and missing or stale installs are noted"
  (let ([p (make-project "layout-akku"
             (list (cons "src/a.sls" (library-text "(a)"))
                   (cons "src/b.sls" (library-text "(b)"))
                   (cons "src/c.sls" (library-text "(c)"))
                   (cons ".akku/lib/c.sls" (string-append (library-text "(c)") ";; an older copy\n"))))])
    (system (format "ln -s ../../src/a.sls '~a'" (project-file p ".akku/lib/a.sls")))
    (let ([layout (layout-of p)])
      (check-equal? 'akku (project-layout-source layout))
      (check-equal? (under p ".akku/lib") (project-layout-library-directories layout))
      (check-equal? '() (project-layout-ambiguous layout))
      (check-equal?
        (list (format "(b) is defined in ~a but is not installed in ~a; run akku install"
                (project-file p "src/b.sls") (project-file p ".akku/lib"))
              (format "Akku's copy of (c), ~a, is out of date with ~a; run akku install"
                (project-file p ".akku/lib/c.sls") (project-file p "src/c.sls")))
        (project-layout-notes layout))
      (check-equal? (format "(b) is defined in ~a but is not installed in ~a; run akku install"
                      (project-file p "src/b.sls") (project-file p ".akku/lib"))
        (note-for layout '("b"))))
    (remove-project! p)))

(test "library-directories in the configuration wins, is used exactly and reports no ambiguity"
  (let ([p (make-project "layout-explicit"
             (list (cons ".cz" "(root) (library-directories \"src\" \"other\")\n")
                   (cons "src/a.sls" (library-text "(a)"))
                   (cons "other/a.sls" (library-text "(a)"))
                   (cons "tests/t.sls" (library-text "(t)"))))])
    (let ([layout (layout-of p)])
      (check-equal? 'explicit (project-layout-source layout))
      (check-equal? (under p "src" "other") (project-layout-library-directories layout))
      (check-equal? '() (project-layout-ambiguous layout))
      (check-equal? (format "(t) is defined in ~a, which Chez finds only with ~a as a library directory; add it to library-directories"
                      (project-file p "tests/t.sls") (project-file p "tests"))
        (note-for layout '("t"))))
    (remove-project! p)))

(test "a name two roots supply is ambiguous, one file reached twice is not, and exclude removes a copy"
  (let ([p (make-project "layout-clash"
             (list (cons "src/a.sls" (library-text "(a)"))
                   (cons "src/b.sls" (library-text "(b)"))
                   (cons "test/data/a.sls" (library-text "(a)"))))])
    (mkdir (project-file p "linked"))
    (system (format "ln -s ../src/b.sls '~a'" (project-file p "linked/b.sls")))
    (let ([outside (make-project "layout-clash-outside" (list (cons "lib/a.sls" (library-text "(a)"))))])
      (system (format "ln -s '~a' '~a'" (project-file outside "lib") (project-file p "outside")))
      (check-equal? (under p "linked" "src" "test/data") (project-layout-library-directories (layout-of p)))
      (remove-project! outside))
    (let ([layout (layout-of p)])
      (check-equal? (under p "linked" "src" "test/data") (project-layout-library-directories layout))
      (check-equal? (list (cons '("a") (under p "src/a.sls" "test/data/a.sls"))) (ambiguous-files layout))
      (check-equal? (list (string-append "ambiguous (a): " (project-file p "src/a.sls") " in root " (project-file p "src")
                            ", " (project-file p "test/data/a.sls") " in root " (project-file p "test/data")))
        (filter (lambda (line) (contains? "ambiguous" line)) (project-layout->lines layout))))
    (write-project-file! p ".cz" "(exclude \"test/data/**\")\n")
    (check-equal? '() (project-layout-ambiguous (layout-of p)))
    (remove-project! p)))

(test "inference enters listed submodules and stops at other repositories and nested projects"
  (let ([p (make-project "layout-repositories"
             (list (cons ".cz" "")
                   (cons ".gitmodules" "[submodule \"vendor/v\"]\n\tpath = vendor/v\n")
                   (cons "src/a.sls" (library-text "(a)"))
                   (cons "vendor/v/.git" "gitdir: ../../.git/modules/vendor/v\n")
                   (cons "vendor/v/lib/v.sls" (library-text "(v)"))
                   (cons "checkout/.git" "gitdir: ../.git/worktrees/checkout\n")
                   (cons "checkout/src/a.sls" (library-text "(a)"))
                   (cons "nested/.cz" "(root)\n")
                   (cons "nested/src/a.sls" (library-text "(a)"))))])
    (mkdir (project-file p ".git"))
    (let ([layout (layout-of p)])
      (check-equal? (under p "src" "vendor/v/lib") (project-layout-library-directories layout))
      (check-equal? '() (project-layout-ambiguous layout)))
    (write-project-file! p ".cz" "(exclude \"vendor/v/**\")\n")
    (check-equal? (under p "src") (project-layout-library-directories (layout-of p)))
    (remove-project! p)))

(test "a layout is current until a library file or a directory changes, and is recomputed from the changes"
  (let ([p (make-project "layout-stamps"
             (list (cons ".cz" "(root)\n")
                   (cons "src/a.sls" (library-text "(a)"))))])
    (let ([layout (layout-of p)])
      (check (project-layout-current? layout))
      (write-project-file! p "lib/b.sls" (library-text "(b)"))
      (check (not (project-layout-current? layout)))
      (let ([next (compute-project-layout (project-layout-config layout) layout)])
        (check (project-layout-current? next))
        (check-equal? (under p "lib" "src") (project-layout-library-directories next))
        (check (not (project-layout-same? layout next)))
        (write-project-file! p "src/a.sls" (library-text "(z)"))
        (check (not (project-layout-current? next)))
        (check-equal? (under p "lib") (project-layout-library-directories
                                        (compute-project-layout (project-layout-config next) next)))))
    (remove-project! p)))

(test "the published layout says how it was obtained"
  (let ([p (make-project "layout-report"
             (list (cons ".cz" "(root)\n")
                   (cons "src/a.sls" (library-text "(a)"))))])
    (let ([layout (layout-of p)])
      (check-equal?
        (list (format "project ~a (found by its .cz file)" (project-root p))
              (format "library directories (inferred from library headers): ~a" (project-file p "src"))
              "library extensions: .chezscheme.sls .ss .sls .scm .sch")
        (project-layout->lines layout))
      (check-equal?
        `((root . ,(project-root p)) (rootMarker . "configuration") (source . "inferred")
          (libraryDirectories . #(,(project-file p "src")))
          (libraryExtensions . #(".chezscheme.sls" ".ss" ".sls" ".scm" ".sch"))
          (ambiguous . #()) (notes . #()))
        (project-layout->json layout)))
    (remove-project! p)))

(test "one name with two extensions in one root is settled by Chez's extension order, and percent-encoded names give no root"
  (let ([p (make-project "layout-extensions"
             (list (cons "helpers.chezscheme.sls" (library-text "(helpers)"))
                   (cons "helpers.ss" (library-text "(helpers)"))
                   (cons "srfi/%3a1.sls" (library-text "(srfi :1)"))))])
    (let ([layout (layout-of p)])
      (check-equal? (list (project-root p)) (project-layout-library-directories layout))
      (check-equal? '() (project-layout-ambiguous layout))
      (check-equal? (list #f)
        (map library-file-root (filter (lambda (f) (equal? (library-file-name f) '("srfi" ":1")))
                                       (project-layout-library-files layout)))))
    (remove-project! p)))

(test "two checkouts side by side are two projects, with no ambiguity between them"
  (let ([p (make-project "layout-checkouts"
             (list (cons "one/.git" "gitdir: /elsewhere/.git/worktrees/one\n")
                   (cons "one/src/a.sls" (library-text "(a)"))
                   (cons "two/.git" "gitdir: /elsewhere/.git/worktrees/two\n")
                   (cons "two/src/a.sls" (library-text "(a)"))))])
    (let ([layout (compute-project-layout (find-project-config (project-file p "one/src/a.sls") (project-root p)) #f)])
      (check-equal? (project-file p "one") (project-layout-root layout))
      (check-equal? (under p "one/src") (project-layout-library-directories layout))
      (check-equal? '() (project-layout-ambiguous layout)))
    (remove-project! p)))

(test "a parent with two submodules is one project with one layout, wherever the search starts"
  (let ([p (make-project "layout-parent"
             (list (cons ".cz" "")
                   (cons ".gitmodules" "[submodule \"one\"]\n\tpath = one\n[submodule \"two\"]\n\tpath = two\n")
                   (cons "one/lib/x.sls" (library-text "(x)"))
                   (cons "two/lib/y.sls" "(library (y) (export) (import (chezscheme) (x)))\n")))])
    (for-each mkdir (under p ".git" "one/.git" "two/.git"))
    (let ([layouts (map (lambda (name)
                          (compute-project-layout (find-project-config (project-file p name) "/") #f))
                        '("x.ss" "one/lib/x.sls" "two/lib/y.sls"))])
      (check-equal? (list (project-root p) (project-root p) (project-root p)) (map project-layout-root layouts))
      (check-equal? (list (under p "one/lib" "two/lib") (under p "one/lib" "two/lib") (under p "one/lib" "two/lib"))
        (map project-layout-library-directories layouts)))
    (remove-project! p)))

(test "CHEZSCHEMELIBDIRS set but not named by the configuration gives one note"
  (let ([p (make-project "layout-environment" (list (cons "src/a.sls" (library-text "(a)"))))])
    (putenv "CHEZSCHEMELIBDIRS" "/x/lib")
    (check-equal? '("CHEZSCHEMELIBDIRS is set to /x/lib and ignored; to use it, write (environment-variable \"CHEZSCHEMELIBDIRS\") in library-directories")
      (project-layout-notes (layout-of p)))
    (write-project-file! p ".cz" "(library-directories \"src\" (environment-variable \"CHEZSCHEMELIBDIRS\"))\n")
    (check-equal? '() (project-layout-notes (layout-of p)))
    (putenv "CHEZSCHEMELIBDIRS" "")
    (remove-project! p)))

(test "without .git or .cz, inference stops after 20000 files and directories and says so"
  (let ([p (make-project "layout-unbounded" '())])
    (system (format "mkdir -p '~a/many' && cd '~a/many' && seq 1 20001 | xargs touch" (project-root p) (project-root p)))
    (write-project-file! p "src/a.sls" (library-text "(a)"))
    (let ([layout (layout-of p)])
      (check-equal? '() (project-layout-library-directories layout))
      (check-equal? (list (format "~a has no .git or .cz; library inference stopped after 20000 files and directories; add a .cz with (root)"
                            (project-root p)))
        (project-layout-notes layout)))
    (write-project-file! p ".cz" "(root)\n")
    (check-equal? (under p "src") (project-layout-library-directories (layout-of p)))
    (remove-project! p)))

(test "a directory that cannot be read is named in the notes, and the rest of the layout is computed"
  (let ([p (make-project "layout-unreadable" (list (cons ".cz" "(root)\n") (cons "src/a.sls" (library-text "(a)"))
                                                   (cons "locked/b.sls" (library-text "(b)"))))])
    (chmod (project-file p "locked") 0)
    (let ([layout (layout-of p)])
      (check-equal? (under p "src") (project-layout-library-directories layout))
      (check-equal? (list (format "~a could not be read (permission denied); libraries under it are not inferred"
                            (project-file p "locked")))
        (project-layout-notes layout)))
    (chmod (project-file p "locked") #o755)
    (remove-project! p)))

(test "a layout stays current while a directory stays unreadable, and is not current once that changes"
  (let ([p (make-project "layout-unreadable-current" (list (cons ".cz" "(root)\n") (cons "src/a.sls" (library-text "(a)"))
                                                           (cons "locked/b.sls" (library-text "(b)"))))])
    (let ([readable (layout-of p)])
      (chmod (project-file p "locked") 0)
      (check (not (project-layout-current? readable)))
      (check (config-current? (project-layout-config readable)))
      (let ([locked (compute-project-layout (project-layout-config readable) readable)])
        (check (project-layout-current? locked))
        (check (not (project-layout-same? readable locked)))
        (check (not (project-layout-same-for-analysis? readable locked)))
        (chmod (project-file p "locked") #o755)
        (check (not (project-layout-current? locked)))))
    (chmod (project-file p "locked") #o755)
    (remove-project! p)))

(test "layouts that differ only in their notes are the same for an analysis"
  (let ([p (make-project "layout-notes-only" (list (cons ".cz" "(root)\n") (cons "src/a.sls" (library-text "(a)"))))])
    (mkdir (project-file p "locked"))
    (let ([readable (layout-of p)])
      (chmod (project-file p "locked") 0)
      (let ([locked (layout-of p)])
        (check (not (project-layout-same? readable locked)))
        (check (project-layout-same-for-analysis? readable locked))))
    (chmod (project-file p "locked") #o755)
    (remove-project! p)))

(test-exit)
