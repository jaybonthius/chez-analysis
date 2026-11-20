(library (analysis project)
  (export
    ;; layouts
    compute-project-layout project-layout? project-layout-config project-layout-root
    project-layout-source project-layout-library-directories project-layout-library-extensions
    project-layout-library-files project-layout-ambiguous project-layout-notes
    project-layout-stamps project-layout-absent project-layout-current? project-layout-same?
    project-layout-same-for-analysis?
    layout-source layout-source-set
    ;; library files
    library-file? library-file-path library-file-name library-file-root library-file-start
    library-file-end
    ;; reporting
    library-candidates unresolved-import-note project-layout->json project-layout->lines)
  (import (chezscheme) (analysis config) (analysis reader) (analysis path)
          (analysis posix) (analysis text))

  (define-enumeration layout-source (explicit akku inferred) layout-source-set)

  ;; root is #f when the path does not end in the name, so Chez can never
  ;; find the file under that name.
  (define-record-type library-file
    (nongenerative) (sealed #t)
    (fields path name root start end))

  ;; The library headers of one file, kept with the stamp they were read at,
  ;; so a later computation reads only the files that changed.
  (define-record-type reading
    (nongenerative) (sealed #t)
    (fields path stamp headers))

  (define-record-type header
    (nongenerative) (sealed #t)
    (fields name start end))

  (define-record-type project-layout
    (nongenerative) (sealed #t)
    (fields config source library-directories library-extensions library-files ambiguous notes
            stamps absent readings))

  (define (project-layout-root layout) (config-root (project-layout-config layout)))

  (define akku-directory ".akku/lib")

  ;; Without .git or .chez-tools the project is a directory the user happens
  ;; to be in, which may be a home directory.
  (define fallback-entry-limit 20000)

  (define (in-directory dir name)
    (if (string=? dir "/") (string-append "/" name) (string-append dir "/" name)))

  (define (string-suffix? suffix s)
    (let ([n (string-length suffix)] [m (string-length s)])
      (and (fx<= n m) (string=? suffix (substring s (fx- m n) m)))))

  (define (library-path name)
    (fold-left (lambda (acc part) (string-append acc "/" part)) (car name) (cdr name)))

  (define (inside? path dir) (and (path-relative-to path dir) #t))

  (define (read-bytes path)
    (let ([bv (call-with-port (open-file-input-port path) get-bytevector-all)])
      (if (eof-object? bv) (make-bytevector 0) bv)))

  (define (condition-text c)
    (if (and (condition? c) (message-condition? c)) (condition-message c) (format "~s" c)))

  (define (read-headers path)
    (let ([c (read-cst (make-text (decode-source-bytes (read-bytes path)))
               (make-reader-mode (syntax-mode r6rs) (fold-mode no-fold)))])
      (filter values
        (map (lambda (f)
               (let ([h (cst-library-header c (top-level-form-node f))])
                 (and h
                      (let ([x (library-header-name-node h)])
                        (make-header (library-header-name h)
                          (if (token? x) (token-start x) (node-start x))
                          (if (token? x) (token-end x) (node-end x)))))))
             (vector->list (cst-top-level-forms c))))))

  ;; The inverse of Chez's search: a file found under ROOT/a/b/c.EXT names
  ;; (a b c), so a file whose path, less one of the extensions, ends in its
  ;; header's name has the rest of the path as its root, and any other file
  ;; cannot be found by name at all.
  (define (library-root path name extensions)
    (let ([tail (string-append "/" (library-path name))])
      (let loop ([extensions extensions])
        (cond
          [(null? extensions) #f]
          [(and (string-suffix? (car extensions) path)
                (let ([stem (substring path 0 (fx- (string-length path) (string-length (car extensions))))])
                  (and (string-suffix? tail stem)
                       (let ([root (substring stem 0 (fx- (string-length stem) (string-length tail)))])
                         (if (string=? root "") "/" root)))))]
          [else (loop (cdr extensions))]))))

  (define (library-file<? a b)
    (or (string<? (library-file-path a) (library-file-path b))
        (and (string=? (library-file-path a) (library-file-path b))
             (fx< (library-file-start a) (library-file-start b)))))

  ;; A nested checkout or worktree is another project; only a listed
  ;; submodule is part of this one.
  (define (submodule-paths dir note-stamp!)
    (let-values ([(dirs stamp) (repository-submodules dir)])
      (note-stamp! (in-directory dir ".gitmodules") stamp)
      dirs))

  (define (enclosing-repository dir)
    (let loop ([dir dir])
      (cond
        [(file-exists? (in-directory dir ".git")) dir]
        [(string=? dir "/") #f]
        [else (loop (path-parent dir))])))

  (define (compute-project-layout config previous)
    (unless (config? config) (assertion-violationf 'compute-project-layout "~s is not a configuration" config))
    (unless (or (not previous) (project-layout? previous))
      (assertion-violationf 'compute-project-layout "~s is not a project layout" previous))
    (let* ([root (config-root config)]
           [real-root (or (guard (c [(posix-error? c) #f]) (real-path root)) root)]
           [extensions (config-library-extensions config)]
           [earlier (let ([table (make-hashtable string-hash string=?)])
                      (when previous
                        (for-each (lambda (r) (hashtable-set! table (reading-path r) r)) (project-layout-readings previous)))
                      table)]
           [stamps '()] [absent '()] [notes '()] [readings '()]
           [visited (make-hashtable string-hash string=?)]
           [entry-limit (and (eq? (config-root-marker config) 'fallback) fallback-entry-limit)]
           [entries 0])
      (define (note! fmt . args) (set! notes (cons (apply format fmt args) notes)))
      (define (note-stamp! p stamp)
        (if stamp (set! stamps (cons (cons p stamp) stamps)) (set! absent (cons p absent))))
      (define (read-file! p stamp)
        (let ([old (hashtable-ref earlier p #f)])
          (if (and old (file-stamp=? (reading-stamp old) stamp))
              (set! readings (cons old readings))
              (guard (c [(or (i/o-error? c) (posix-error? c))
                         (note! "~a could not be read: ~a" p (condition-text c))])
                (set! readings (cons (make-reading p stamp (read-headers p)) readings))))))
      ;; Only a symbolic link can lead outside the root, so only a link's
      ;; target is resolved; a link that leaves the project gives #f.
      (define (followed p)
        (if (file-symbolic-link? p)
            (let ([real (guard (c [(posix-error? c) #f]) (real-path p))])
              (and real (inside? real real-root) real))
            p))
      (define (stopped?) (and entry-limit (fx>= entries entry-limit)))
      (define (walk! dir submodules)
        (let ([real (followed dir)])
          (when (and real (not (hashtable-contains? visited real)) (not (stopped?)))
            (hashtable-set! visited real #t)
            (note-stamp! dir (path-stamp dir))
            (let ([names (guard (c [(i/o-error? c) (note! "~a could not be listed: ~a" dir (condition-text c)) #f])
                           (list-sort string<? (directory-list dir)))])
              (when names
                (for-each
                  (lambda (name)
                    (let ([p (in-directory dir name)] [skip? (stopped?)])
                      (unless skip? (set! entries (fx+ entries 1)))
                      (cond
                        [skip? (void)]
                        ;; Hidden directories hold version control, Akku's
                        ;; installed copies and tools' state, not the
                        ;; project's own libraries.
                        [(char=? (string-ref name 0) #\.) (void)]
                        [(config-excluded? config p) (void)]
                        [(file-directory? p)
                         ;; The directory's own stamp changes when a file
                         ;; is created in it, so only a file that exists
                         ;; needs one. Chez could not load a library from
                         ;; a directory that cannot be read either.
                         (let* ([config-file (in-directory p config-file-name)] [stamp (path-stamp config-file)])
                           (when stamp (note-stamp! config-file stamp))
                           (cond
                             [(unreadable-stamp? stamp)
                              (note! "~a could not be read (~a); libraries under it are not inferred"
                                p (unreadable-stamp-reason stamp))]
                             [(and stamp (config-file-root? config-file)) (void)]
                             [(not (file-exists? (in-directory p ".git"))) (walk! p submodules)]
                             [(member p submodules) (walk! p (append (submodule-paths p note-stamp!) submodules))]
                             [else (void)]))]
                        [(exists (lambda (ext) (string-suffix? ext name)) extensions)
                         (let ([stamp (path-stamp p)])
                           (cond
                             [(unreadable-stamp? stamp)
                              (note-stamp! p stamp)
                              (note! "~a could not be read (~a)" p (unreadable-stamp-reason stamp))]
                             [(and stamp (followed p))
                              (note-stamp! p stamp)
                              (read-file! p stamp)]))])))
                  names))))))
      (for-each (lambda (f) (note-stamp! (car f) (cdr f))) (config-files config))
      (for-each (lambda (p) (note-stamp! p #f)) (config-absent-files config))
      (for-each (lambda (n) (note! "~a" n)) (config-notes config))
      (walk! root (let ([repository (enclosing-repository root)])
                    (if repository (submodule-paths repository note-stamp!) '())))
      (when (stopped?)
        (note! "~a has no .git or .chez-tools; library inference stopped after ~a files and directories; add a .chez-tools with (root)"
          root entries))
      (let* ([files (list-sort library-file<?
                      (apply append
                        (map (lambda (r)
                               (map (lambda (h)
                                      (make-library-file (reading-path r) (header-name h)
                                        (library-root (reading-path r) (header-name h) extensions)
                                        (header-start h) (header-end h)))
                                    (reading-headers r)))
                             readings)))]
             [akku (in-directory root akku-directory)]
             [source (cond
                       [(config-library-directories config) (layout-source explicit)]
                       [(file-directory? akku) (layout-source akku)]
                       [else (layout-source inferred)])]
             [project-files (filter (lambda (f) (and (library-file-root f) (inside? (library-file-root f) root))) files)]
             [directories (case source
                            [(explicit) (config-library-directories config)]
                            [(akku) (list akku)]
                            [else (list-sort string<? (unique (map library-file-root project-files)))])]
             [ambiguous (if (eq? source (layout-source inferred)) (ambiguous-names project-files) '())])
        (when (eq? source (layout-source inferred)) (note-stamp! akku #f))
        (when (eq? source (layout-source akku))
          (note-stamp! akku (path-stamp akku))
          (check-akku-installs! files akku extensions note! note-stamp!))
        (let ([libdirs (getenv "CHEZSCHEMELIBDIRS")])
          (when (and libdirs (not (string=? libdirs "")) (not (assoc "CHEZSCHEMELIBDIRS" (config-environment config))))
            (note! "CHEZSCHEMELIBDIRS is set to ~a and ignored; to use it, write (environment-variable \"CHEZSCHEMELIBDIRS\") in library-directories"
              libdirs)))
        (make-project-layout config source directories extensions files ambiguous (reverse notes)
          (reverse stamps) (reverse absent) readings))))

  (define (unique strings)
    (let loop ([strings strings] [acc '()])
      (cond
        [(null? strings) (reverse acc)]
        [(member (car strings) acc) (loop (cdr strings) acc)]
        [else (loop (cdr strings) (cons (car strings) acc))])))

  ;; Two paths are one file when their real paths are equal, as Akku's
  ;; links to a project's files are; that is the only use of resolved links.
  (define (distinct-files files)
    (let loop ([files files] [seen '()] [acc '()])
      (if (null? files)
          (reverse acc)
          (let* ([path (library-file-path (car files))]
                 [real (or (guard (c [(posix-error? c) #f]) (real-path path)) path)])
            (if (member real seen)
                (loop (cdr files) seen acc)
                (loop (cdr files) (cons real seen) (cons (car files) acc)))))))

  (define (ambiguous-names files)
    (let ([by-name (make-hashtable equal-hash equal?)])
      (for-each (lambda (f) (hashtable-update! by-name (library-file-name f) (lambda (fs) (cons f fs)) '())) files)
      (list-sort (lambda (a b) (string<? (library-path (car a)) (library-path (car b))))
        (filter values
          (map (lambda (name)
                 (let ([candidates (reverse (hashtable-ref by-name name '()))])
                   (and (pair? (cdr candidates))
                        (let ([distinct (distinct-files candidates)])
                          (and (fx> (length (unique (map library-file-root distinct))) 1)
                               (cons name distinct))))))
               (vector->list (hashtable-keys by-name)))))))

  ;; Akku installs each library of the project under .akku/lib by its name,
  ;; as a link to the file or as a copy; a missing or different entry means
  ;; scheme run in the project would not see the file as it is now.
  (define (check-akku-installs! files akku extensions note! note-stamp!)
    (for-each
      (lambda (f)
        (when (and (library-file-root f) (not (inside? (library-file-path f) akku)))
          (let* ([base (in-directory akku (library-path (library-file-name f)))]
                 [installed (find (lambda (p) (file-exists? p)) (map (lambda (ext) (string-append base ext)) extensions))])
            (for-each (lambda (ext) (let ([p (string-append base ext)]) (note-stamp! p (path-stamp p)))) extensions)
            (cond
              [(not installed)
               (note! "(~{~a~^ ~}) is defined in ~a but is not installed in ~a; run akku install"
                 (library-file-name f) (library-file-path f) akku)]
              [(guard (c [(or (i/o-error? c) (posix-error? c)) #f])
                 (and (not (equal? (real-path installed) (real-path (library-file-path f))))
                      (not (bytevector=? (read-bytes installed) (read-bytes (library-file-path f))))))
               (note! "Akku's copy of (~{~a~^ ~}), ~a, is out of date with ~a; run akku install"
                 (library-file-name f) installed (library-file-path f))]))))
      files))

  (define (project-layout-current? layout)
    (unless (project-layout? layout) (assertion-violationf 'project-layout-current? "~s is not a project layout" layout))
    (and (for-all (lambda (s) (path-stamp=? (path-stamp (car s)) (cdr s))) (project-layout-stamps layout))
         (for-all (lambda (p) (not (path-stamp p))) (project-layout-absent layout))))

  ;; Two layouts that report the same are interchangeable for workers and
  ;; for the client, whatever files were read to compute them.
  (define (project-layout-same? a b)
    (and (project-layout? a) (project-layout? b)
         (equal? (project-layout->lines a) (project-layout->lines b))))

  ;; The notes do not change what an analysis does, and a project whose
  ;; notes vary on every computation would otherwise be analysed without end.
  (define (project-layout-same-for-analysis? a b)
    (define (file-fields f)
      (list (library-file-path f) (library-file-name f) (library-file-root f) (library-file-start f) (library-file-end f)))
    (and (project-layout? a) (project-layout? b)
         (eq? (project-layout-source a) (project-layout-source b))
         (equal? (project-layout-library-directories a) (project-layout-library-directories b))
         (equal? (project-layout-library-extensions a) (project-layout-library-extensions b))
         (equal? (map file-fields (project-layout-library-files a)) (map file-fields (project-layout-library-files b)))
         (equal? (map (lambda (entry) (cons (car entry) (map file-fields (cdr entry)))) (project-layout-ambiguous a))
                 (map (lambda (entry) (cons (car entry) (map file-fields (cdr entry)))) (project-layout-ambiguous b)))))

  (define (library-name->string name) (format "(~{~a~^ ~})" name))

  ;; Spelt as the worker spells the paths its search tried, so that the two
  ;; can be compared.
  (define (library-candidates layout name)
    (unless (project-layout? layout) (assertion-violationf 'library-candidates "~s is not a project layout" layout))
    (let ([tail (library-path name)])
      (apply append
        (map (lambda (dir)
               (map (lambda (ext) (string-append dir "/" tail ext)) (project-layout-library-extensions layout)))
             (project-layout-library-directories layout)))))

  (define (unresolved-import-note layout name)
    (unless (project-layout? layout) (assertion-violationf 'unresolved-import-note "~s is not a project layout" layout))
    (let* ([root (project-layout-root layout)]
           [files (filter (lambda (f) (equal? (library-file-name f) name)) (project-layout-library-files layout))]
           [directories (project-layout-library-directories layout)]
           [akku? (eq? (project-layout-source layout) (layout-source akku))])
      (cond
        [(find (lambda (f) (and (library-file-root f) (inside? (library-file-root f) root)
                                (not (member (library-file-root f) directories))))
               files)
         => (lambda (f)
              (values
                (if akku?
                    (format "~a is defined in ~a but is not installed in ~a; run akku install"
                      (library-name->string name) (library-file-path f) (car directories))
                    (format "~a is defined in ~a, which Chez finds only with ~a as a library directory; add it to library-directories"
                      (library-name->string name) (library-file-path f) (library-file-root f)))
                f))]
        [(find library-file-root files)
         => (lambda (f)
              (values
                (format "~a is defined in ~a, which Chez finds only with ~a, outside the project, as a library directory; add it to library-directories or use Akku"
                  (library-name->string name) (library-file-path f) (library-file-root f))
                f))]
        [(and (pair? files) (car files))
         => (lambda (f)
              (values
                (format "~a is defined in ~a, whose path does not end in ~a with a library extension, so Chez cannot find it by name"
                  (library-name->string name) (library-file-path f) (library-path name))
                f))]
        [else (values #f #f)])))

  (define (project-layout->lines layout)
    (unless (project-layout? layout) (assertion-violationf 'project-layout->lines "~s is not a project layout" layout))
    (let ([config (project-layout-config layout)])
      (append
        (list (format "project ~a (found by ~a)" (config-root config)
                (case (config-root-marker config)
                  [(configuration) "its .chez-tools file"]
                  [(git) "its git repository"]
                  [else "the fallback directory"]))
              (format "library directories (~a): ~{~a~^ ~}"
                (case (project-layout-source layout)
                  [(explicit) "from library-directories"]
                  [(akku) "Akku's"]
                  [else "inferred from library headers"])
                (project-layout-library-directories layout))
              (format "library extensions: ~{~a~^ ~}" (project-layout-library-extensions layout)))
        (map (lambda (entry)
               (format "ambiguous ~a: ~{~a~^, ~}" (library-name->string (car entry))
                 (map (lambda (f) (format "~a in root ~a" (library-file-path f) (library-file-root f))) (cdr entry))))
             (project-layout-ambiguous layout))
        (map (lambda (n) (string-append "note: " n)) (project-layout-notes layout)))))

  (define (project-layout->json layout)
    (unless (project-layout? layout) (assertion-violationf 'project-layout->json "~s is not a project layout" layout))
    (let ([config (project-layout-config layout)])
      (list (cons 'root (config-root config))
            (cons 'rootMarker (symbol->string (config-root-marker config)))
            (cons 'source (symbol->string (project-layout-source layout)))
            (cons 'libraryDirectories (list->vector (project-layout-library-directories layout)))
            (cons 'libraryExtensions (list->vector (project-layout-library-extensions layout)))
            (cons 'ambiguous
              (list->vector
                (map (lambda (entry)
                       (list (cons 'library (list->vector (car entry)))
                             (cons 'files
                               (list->vector
                                 (map (lambda (f) (list (cons 'path (library-file-path f)) (cons 'root (library-file-root f))))
                                      (cdr entry))))))
                     (project-layout-ambiguous layout))))
            (cons 'notes (list->vector (project-layout-notes layout)))))))
