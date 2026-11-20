(library (analysis config)
  (export
    ;; finding and reading
    find-project-config read-config-file default-project-config config-file-name
    config-file-root? repository-submodules
    ;; configurations
    config? config-root config-root-marker config-files config-absent-files config-notes config-current?
    config-library-directories config-library-extensions config-source-extensions
    config-file-kind config-reader-mode config-case-sensitive? config-preload
    config-excluded? config-source-files config-definition-forms config-syntax-binding-forms
    config-lint config-format config-worker-limits config-environment
    file-kind file-kind-set root-marker root-marker-set
    ;; worker limits
    worker-limits? worker-limits-memory worker-limits-memory-budget worker-limits-timeout
    worker-limits-analyses worker-limits-foreground
    ;; lint settings
    lint-settings? lint-settings-switches lint-settings-severities lint-settings-ignores
    lint-settings-unused-names lint-settings-undefined-variable-warnings?
    lint-settings-mentions
    rule-mention? rule-mention-name rule-mention-path rule-mention-start rule-mention-end
    lint-ignore? lint-ignore-glob lint-ignore-rules
    ;; errors
    &config-error make-config-error config-error? config-error-path config-error-start
    config-error-end)
  (import (chezscheme) (analysis reader) (analysis path) (analysis text) (analysis posix)
          (analysis diagnostic))

  (define-enumeration file-kind (library program script include) file-kind-set)
  (define-enumeration root-marker (configuration git fallback) root-marker-set)

  (define config-file-name ".chez-tools")

  (define-condition-type &config-error &error make-config-error config-error?
    (path config-error-path) (start config-error-start) (end config-error-end))

  (define-record-type worker-limits
    (nongenerative) (sealed #t)
    (fields memory memory-budget timeout analyses foreground))

  (define-record-type rule-mention
    (nongenerative) (sealed #t)
    (fields name path start end))

  (define-record-type lint-ignore
    (nongenerative) (sealed #t)
    (fields glob rules))

  (define-record-type lint-settings
    (nongenerative) (sealed #t)
    (fields switches severities ignores unused-names undefined-variable-warnings? mentions))

  (define-record-type config
    (nongenerative) (sealed #t)
    (fields root root-marker files absent-files notes library-directories library-extensions
            source-extensions excludes file-kinds reader-modes case-sensitive? preload definition-forms
            syntax-binding-forms lint format worker-limits environment))

  (define (config-current? config)
    (unless (config? config) (assertion-violationf 'config-current? "~s is not a configuration" config))
    (and (for-all (lambda (f) (path-stamp=? (path-stamp (car f)) (cdr f))) (config-files config))
         (for-all (lambda (p) (not (path-stamp p))) (config-absent-files config))))

  (define default-library-extensions '(".chezscheme.sls" ".ss" ".sls" ".scm" ".sch"))
  (define default-source-extensions '(".sls" ".sps" ".ss" ".scm"))
  (define default-definition-forms
    '("library" "define" "define-syntax" "define-values" "define-record-type"
      "define-condition-type" "define-enumeration" "define-ftype" "define-property" "module"
      "meta" "define-record"))
  (define default-syntax-binding-forms
    '("define-syntax" "let-syntax" "letrec-syntax" "fluid-let-syntax" "alias"))
  (define default-unused-names '("_" "_*" "ignore"))

  (define mebibyte (* 1024 1024))

  (define-record-type item
    (nongenerative) (sealed #t)
    (fields value start end))

  (define (config-error path start end fmt . args)
    (raise (condition (make-config-error path start end) (make-who-condition 'chez-tools)
             (make-message-condition (apply format fmt args)))))

  ;; The file is data: only these forms are accepted, and nothing in it is
  ;; evaluated or read by Chez's reader.
  (define (cst->items c path)
    (define (convert x)
      (define (bad)
        (config-error path (if (token? x) (token-start x) (node-start x))
          (if (token? x) (token-end x) (node-end x))
          "~a is not data a configuration file can hold"
          (text-substring (cst-text c) (if (token? x) (token-start x) (node-start x))
            (if (token? x) (token-end x) (node-end x)))))
      (cond
        [(token? x)
         (let ([text (token-text c x)] [start (token-start x)] [end (token-end x)])
           (case (token-kind x)
             [(symbol) (make-item (string->symbol (symbol-token-name c x)) start end)]
             [(string) (make-item (string-token-value c x) start end)]
             [(boolean)
              (cond
                [(member text '("#t" "#true")) (make-item #t start end)]
                [(member text '("#f" "#false")) (make-item #f start end)]
                [else (bad)])]
             [(number)
              (if (and (fx> (string-length text) 0)
                       (for-all char-numeric? (string->list (if (char=? (string-ref text 0) #\-)
                                                                (substring text 1 (string-length text))
                                                                text)))
                       (not (string=? text "-")))
                  (make-item (string->number text 10) start end)
                  (bad))]
             [else (bad)]))]
        [(and (eq? (node-kind x) 'list)
              (memq (token-subkind (vector-ref (node-children x) 0)) '(paren bracket))
              (not (exists (lambda (child) (and (token? child) (eq? (token-kind child) 'dot)))
                           (vector->list (node-children x)))))
         (make-item (map convert (vector->list (node-data x))) (node-start x) (node-end x))]
        [else (bad)]))
    (let ([diagnostics (cst-diagnostics c)])
      (when (fx> (vector-length diagnostics) 0)
        (let ([d (vector-ref diagnostics 0)])
          (config-error path (diagnostic-start d) (diagnostic-end d) "~a" (diagnostic-message d)))))
    (map (lambda (f) (convert (top-level-form-node f))) (vector->list (cst-top-level-forms c))))

  (define (entry-name it)
    (let ([v (item-value it)])
      (and (pair? v) (symbol? (item-value (car v))) (item-value (car v)))))

  (define-record-type file-entries
    (nongenerative) (sealed #t)
    (fields path dir root? singles layout-items file-kinds reader-modes worker lint format environment))

  (define layout-keys '(library-directories library-extensions exclude))

  (define (decode-file path text)
    (let* ([c (read-cst (make-text text) (make-reader-mode (syntax-mode chezscheme) (fold-mode no-fold)))]
           [items (cst->items c path)]
           [dir (path-parent path)]
           [singles '()] [layout-items '()] [file-kinds '()] [reader-modes '()] [worker '()] [lint '()] [format-entries '()]
           [environment '()] [root? #f])
      (define (fail it fmt . args)
        (apply config-error path (item-start it) (item-end it) fmt args))
      (define (arguments it) (cdr (item-value it)))
      (define (strings it)
        (map (lambda (a)
               (unless (string? (item-value a)) (fail a "expected a string"))
               (item-value a))
             (arguments it)))
      (define (symbols it)
        (map (lambda (a)
               (unless (symbol? (item-value a)) (fail a "expected a symbol"))
               (item-value a))
             (arguments it)))
      (define (one it ok? what)
        (let ([args (arguments it)])
          (unless (and (pair? args) (null? (cdr args)) (ok? (item-value (car args))))
            (fail it "~a takes one ~a" (entry-name it) what))
          (item-value (car args))))
      (define (count? x) (and (integer? x) (exact? x) (> x 0)))
      (define (absolute p) (normalise-path (if (and (fx> (string-length p) 0) (char=? (string-ref p 0) #\/)) p (string-append dir "/" p))))
      (define (add-single! it key value)
        (when (assq key singles) (fail it "~a appears twice in this file" key))
        (when (memq key layout-keys) (set! layout-items (cons it layout-items)))
        (set! singles (cons (cons key value) singles)))
      (define (library-directories it)
        (apply append
          (map (lambda (a)
                 (let ([v (item-value a)])
                   (cond
                     [(string? v) (list (absolute v))]
                     [(and (list? v) (fx= (length v) 2) (eq? (item-value (car v)) 'environment-variable)
                           (string? (item-value (cadr v))))
                      (let ([value (getenv (item-value (cadr v)))])
                        (set! environment (cons (cons (item-value (cadr v)) value) environment))
                        (map absolute (environment-directories (or value ""))))]
                     [else (fail a "expected a directory or (environment-variable \"NAME\")")])))
               (arguments it))))
      (define (glob-rule it values what)
        (let ([args (arguments it)])
          (unless (and (fx= (length args) 2) (string? (item-value (car args)))
                       (memq (item-value (cadr args)) values))
            (fail it "expected (~a \"glob\" ~a)" (entry-name it) what))
          (cons (compile-glob (item-value (car args)) dir) (item-value (cadr args)))))
      (define (sub-entries it decode!)
        (let ([seen '()])
          (for-each
            (lambda (sub)
              (let ([name (entry-name sub)])
                (unless name (fail sub "expected an entry of ~a" (entry-name it)))
                (decode! sub name (lambda (key)
                                    (when (memq key seen) (fail sub "~a appears twice in ~a" key (entry-name it)))
                                    (set! seen (cons key seen))))))
            (arguments it))))
      (define (mention a)
        (unless (symbol? (item-value a)) (fail a "expected a rule name"))
        (make-rule-mention (item-value a) path (item-start a) (item-end a)))
      (for-each
        (lambda (it)
          (case (entry-name it)
            [(root) (unless (null? (arguments it)) (fail it "root takes nothing")) (set! root? #t)]
            [(library-directories) (add-single! it 'library-directories (library-directories it))]
            [(library-extensions) (add-single! it 'library-extensions (strings it))]
            [(source-extensions) (add-single! it 'source-extensions (strings it))]
            [(exclude) (add-single! it 'exclude (map (lambda (g) (compile-glob g dir)) (strings it)))]
            [(case-sensitive) (add-single! it 'case-sensitive (one it boolean? "boolean"))]
            [(preload) (add-single! it 'preload (map absolute (strings it)))]
            [(definition-forms) (add-single! it 'definition-forms (map symbol->string (symbols it)))]
            [(syntax-binding-forms) (add-single! it 'syntax-binding-forms (map symbol->string (symbols it)))]
            [(file-kind)
             (set! file-kinds (cons (glob-rule it (enum-set->list (enum-set-universe (file-kind-set)))
                                      "library|program|script|include")
                                    file-kinds))]
            [(reader-mode)
             (set! reader-modes (cons (glob-rule it '(r6rs chezscheme) "r6rs|chezscheme") reader-modes))]
            [(worker)
             (sub-entries it
               (lambda (sub name seen!)
                 (case name
                   [(memory-limit memory-budget timeout analyses foreground)
                    (seen! name)
                    (set! worker (cons (cons name (one sub count? "positive integer")) worker))]
                   [else (fail sub "unknown worker entry ~a" name)])))]
            [(lint)
             (sub-entries it
               (lambda (sub name seen!)
                 (case name
                   [(enable disable)
                    (for-each
                      (lambda (a)
                        (let ([m (mention a)])
                          (set! lint (cons (list 'switch m (eq? name 'enable)) lint))))
                      (arguments sub))]
                   [(severity)
                    (let ([args (arguments sub)])
                      (unless (and (fx= (length args) 2) (memq (item-value (cadr args)) '(error warning information hint)))
                        (fail sub "expected (severity rule error|warning|information|hint)"))
                      (set! lint (cons (list 'severity (mention (car args)) (item-value (cadr args))) lint)))]
                   [(ignore)
                    (let ([args (arguments sub)])
                      (unless (and (pair? args) (string? (item-value (car args))) (pair? (cdr args)))
                        (fail sub "expected (ignore \"glob\" rule ...)"))
                      (set! lint (cons (list 'ignore (compile-glob (item-value (car args)) dir) (map mention (cdr args))) lint)))]
                   [(unused-names)
                    (seen! name)
                    (set! lint (cons (list 'unused-names (strings sub)) lint))]
                   [(undefined-variable-warnings)
                    (seen! name)
                    (set! lint (cons (list 'undefined-variable-warnings (one sub boolean? "boolean")) lint))]
                   [else (fail sub "unknown lint entry ~a" name)])))]
            [(format)
             (sub-entries it
               (lambda (sub name seen!)
                 (seen! name)
                 (set! format-entries (cons (cons name (decode-format-entry sub name fail)) format-entries))))]
            [else (if (entry-name it)
                      (fail it "unknown entry ~a" (entry-name it))
                      (fail it "expected an entry such as (library-directories \"src\")"))]))
        items)
      (make-file-entries path dir root? singles (reverse layout-items) (reverse file-kinds) (reverse reader-modes)
        worker (reverse lint) format-entries (reverse environment))))

  (define (decode-format-entry sub name fail)
    (define (args) (cdr (item-value sub)))
    (define (values-of) (map item-value (args)))
    (case name
      [(call-layout)
       (let ([v (values-of)])
         (unless (and (fx= (length v) 1) (memq (car v) '(author align))) (fail sub "expected (call-layout author|align)"))
         (car v))]
      [(head-alone)
       (let ([v (values-of)])
         (unless (and (fx= (length v) 1) (memv (car v) '(1 2))) (fail sub "expected (head-alone 1|2)"))
         (car v))]
      [(comment-lines)
       (let ([v (values-of)])
         (unless (and (fx= (length v) 1) (memq (car v) '(with-code as-code))) (fail sub "expected (comment-lines with-code|as-code)"))
         (car v))]
      [(column-zero-heads)
       (let ([v (values-of)])
         (unless (andmap symbol? v) (fail sub "expected (column-zero-heads name ...)"))
         (map symbol->string v))]
      [(indent)
       (map (lambda (e)
              (let ([v (item-value e)])
                (define (rule? x) (or (not x) (and (integer? x) (exact? x)) (memq x '(let module))))
                (define (names x)
                  (let ([x (item-value x)])
                    (cond
                      [(symbol? x) (list (symbol->string x))]
                      [(and (list? x) (pair? x) (andmap (lambda (y) (symbol? (item-value y))) x))
                       (map (lambda (y) (symbol->string (item-value y))) x)]
                      [else #f])))
                (cond
                  [(and (list? v) (fx= (length v) 2) (names (car v)) (rule? (item-value (cadr v))))
                   (cons (names (car v)) (item-value (cadr v)))]
                  [(and (list? v) (fx>= (length v) 2) (rule? (item-value (car v)))
                        (andmap (lambda (x) (symbol? (item-value x))) (cdr v)))
                   (cons (map (lambda (x) (symbol->string (item-value x))) (cdr v)) (item-value (car v)))]
                  [else (fail e "expected (name N), ((name ...) N) or (N name ...)")])))
            (args))]
      [else (fail sub "unknown format entry ~a" name)]))

  ;; As scheme parses CHEZSCHEMELIBDIRS: colon-separated entries, each a
  ;; source directory optionally followed by :: and an object directory.
  (define (environment-directories value)
    (let loop ([parts (split value #\:)] [acc '()])
      (cond
        [(null? parts) (reverse acc)]
        [(string=? (car parts) "") (loop (cdr parts) acc)]
        [(and (pair? (cdr parts)) (string=? (cadr parts) "") (pair? (cddr parts)))
         (loop (cdddr parts) (cons (car parts) acc))]
        [else (loop (cdr parts) (cons (car parts) acc))])))

  (define (split s c)
    (let loop ([i 0] [start 0] [parts '()])
      (cond
        [(fx= i (string-length s)) (reverse (cons (substring s start i) parts))]
        [(char=? (string-ref s i) c) (loop (fx+ i 1) (fx+ i 1) (cons (substring s start i) parts))]
        [else (loop (fx+ i 1) start parts)])))

  (define (read-file-text path)
    (decode-source-bytes (call-with-port (open-file-input-port path) get-bytevector-all*)))

  (define (get-bytevector-all* p)
    (let ([bv (get-bytevector-all p)]) (if (eof-object? bv) (make-bytevector 0) bv)))

  (define (merge-entries entries root marker files absent notes)
    (define (single key default)
      (let loop ([es entries])
        (cond
          [(null? es) default]
          [(assq key (file-entries-singles (car es))) => cdr]
          [else (loop (cdr es))])))
    (define (worker key default)
      (let loop ([es entries])
        (cond
          [(null? es) default]
          [(assq key (file-entries-worker (car es))) => cdr]
          [else (loop (cdr es))])))
    (define (lint-of kind)
      (apply append (map (lambda (e) (filter (lambda (x) (eq? (car x) kind)) (file-entries-lint e))) entries)))
    (define (lint-single kind default)
      (let ([found (lint-of kind)]) (if (null? found) default (cadar found))))
    (let ([budget-default (quotient (memory-budget-bytes) 2)])
      (make-config root marker files absent notes
        (single 'library-directories #f)
        (single 'library-extensions default-library-extensions)
        (single 'source-extensions default-source-extensions)
        (single 'exclude '())
        (apply append (map file-entries-file-kinds entries))
        (apply append (map file-entries-reader-modes entries))
        (single 'case-sensitive #t)
        (single 'preload '())
        (single 'definition-forms default-definition-forms)
        (single 'syntax-binding-forms default-syntax-binding-forms)
        (make-lint-settings
          (map (lambda (x) (cons (rule-mention-name (cadr x)) (caddr x))) (lint-of 'switch))
          (map (lambda (x) (cons (rule-mention-name (cadr x)) (caddr x))) (lint-of 'severity))
          (map (lambda (x) (make-lint-ignore (cadr x) (map rule-mention-name (caddr x)))) (lint-of 'ignore))
          (lint-single 'unused-names default-unused-names)
          (lint-single 'undefined-variable-warnings #f)
          (append (map cadr (lint-of 'switch)) (map cadr (lint-of 'severity))
                  (apply append (map caddr (lint-of 'ignore)))))
        (apply append
          (map (lambda (e) (file-entries-format e)) entries))
        (make-worker-limits
          (* mebibyte (worker 'memory-limit 4096))
          (let ([b (worker 'memory-budget #f)]) (if b (* mebibyte b) budget-default))
          (worker 'timeout 60)
          (worker 'analyses 100)
          (worker 'foreground 3))
        (apply append (map file-entries-environment entries)))))

  ;; With a configuration error, the defaults stand in until the file named
  ;; by the error changes, so the error's file is kept with its stamp.
  (define default-project-config
    (case-lambda
      [(root) (default-project-config root #f)]
      [(root error)
       (unless (and (string? root) (fx> (string-length root) 0) (char=? (string-ref root 0) #\/))
         (assertion-violationf 'default-project-config "~s is not an absolute directory" root))
       (unless (or (not error) (config-error? error))
         (assertion-violationf 'default-project-config "~s is not a configuration error" error))
       (merge-entries '() (normalise-path root) (root-marker fallback)
         (if error
             (let ([stamp (file-stamp (config-error-path error))])
               (if stamp (list (cons (config-error-path error) stamp)) '()))
             '())
         (if (and error (not (file-stamp (config-error-path error)))) (list (config-error-path error)) '())
         '())]))

  ;; A file that does not decode is not a boundary: its error is reported
  ;; to the documents it configures.
  (define (config-file-root? path)
    (unless (string? path) (assertion-violationf 'config-file-root? "~s is not a path" path))
    (guard (c [(or (config-error? c) (i/o-error? c)) #f])
      (file-entries-root? (decode-file path (read-file-text path)))))

  (define (read-config-file path)
    (unless (string? path) (assertion-violationf 'read-config-file "~s is not a path" path))
    (let* ([path (canonical-path path)] [stamp (file-stamp path)])
      (unless stamp (config-error path 0 0 "~a does not exist" path))
      (let ([entries (decode-file path (read-file-text path))])
        (merge-entries (list entries) (file-entries-dir entries) (root-marker configuration)
          (list (cons path stamp)) '() '()))))

  (define (in-directory dir name)
    (if (string=? dir "/") (string-append "/" name) (string-append dir "/" name)))

  (define (repository? dir) (file-exists? (in-directory dir ".git")))

  (define (trim s)
    (let loop ([start 0] [end (string-length s)])
      (cond
        [(and (fx< start end) (char-whitespace? (string-ref s start))) (loop (fx+ start 1) end)]
        [(and (fx< start end) (char-whitespace? (string-ref s (fx- end 1)))) (loop start (fx- end 1))]
        [else (substring s start end)])))

  ;; git writes each submodule's directory as "path = DIR" in its section.
  (define (gitmodules-paths text)
    (filter values
      (map (lambda (line)
             (let* ([line (trim line)] [n (string-length line)])
               (and (fx> n 4) (string=? (substring line 0 4) "path")
                    (let ([rest (trim (substring line 4 n))])
                      (and (fx> (string-length rest) 0) (char=? (string-ref rest 0) #\=)
                           (let ([value (trim (substring rest 1 (string-length rest)))])
                             (if (and (fx>= (string-length value) 2) (char=? (string-ref value 0) #\")
                                      (char=? (string-ref value (fx- (string-length value) 1)) #\"))
                                 (substring value 1 (fx- (string-length value) 1))
                                 value)))))))
           (split text #\newline))))

  ;; The stamp is the caller's to keep, so that a .gitmodules created or
  ;; edited later is noticed.
  (define (repository-submodules dir)
    (unless (string? dir) (assertion-violationf 'repository-submodules "~s is not a directory" dir))
    (let* ([p (in-directory dir ".gitmodules")] [stamp (file-stamp p)])
      (values
        (if stamp
            (guard (c [(i/o-error? c) '()])
              (map (lambda (m) (normalise-path (in-directory dir m))) (gitmodules-paths (read-file-text p))))
            '())
        stamp)))

  ;; The search stops at a repository unless the repository above lists it
  ;; as a submodule and is a Chez project itself (it has a .chez-tools file
  ;; at its root, or is such a submodule): a repository with no .chez-tools
  ;; may be a container of unrelated projects, and its submodules must not
  ;; be absorbed into it. Every file this depends on is kept with its stamp,
  ;; and every place a file was looked for and not found, so a change to
  ;; any of them is noticed.
  (define (find-project-config path fallback-root)
    (unless (string? path) (assertion-violationf 'find-project-config "~s is not a path" path))
    (let ([files '()] [absent '()])
      (define (note-file! p stamp) (unless (assoc p files) (set! files (cons (cons p stamp) files))))
      (define (note-absent! p) (unless (member p absent) (set! absent (cons p absent))))
      (define (enclosing-repository dir)
        (let loop ([dir dir])
          (and (not (string=? dir "/"))
               (let ([up (path-parent dir)])
                 (if (repository? up) up (loop up))))))
      (define (submodules e)
        (let-values ([(dirs stamp) (repository-submodules e)])
          (let ([p (in-directory e ".gitmodules")])
            (if stamp (note-file! p stamp) (note-absent! p)))
          dirs))
      (define (has-config? e)
        (let* ([p (in-directory e config-file-name)] [stamp (file-stamp p)])
          (if stamp (begin (note-file! p stamp) #t) (begin (note-absent! p) #f))))
      (define (member-repository? d)
        (let ([e (enclosing-repository d)])
          (and e
               (member d (submodules e))
               (or (has-config? e) (member-repository? e))
               #t)))
      ;; A submodule the root file excludes is a project of its own, as if
      ;; the search had stopped at it.
      (define (finish found members stop)
        (let* ([root-entry (and (pair? found) (found-file-entries (car found)))]
               [globs (if root-entry
                          (cond [(assq 'exclude (file-entries-singles root-entry)) => cdr] [else '()])
                          '())]
               [excluded (filter (lambda (m) (exists (lambda (g) (glob-match? g m)) globs)) members)])
          (if (pair? excluded)
              (let* ([outermost (car (last-pair excluded))]
                     [passed (fx- (length members) (length (cdr (member outermost members))) 1)])
                (finish (filter (lambda (f) (fx<= (found-file-members-passed f) passed)) found)
                        (list-head members passed)
                        outermost))
              (let ([member-count (length members)] [notes '()])
                (define (role-checked f)
                  (let ([entry (found-file-entries f)])
                    (cond
                      [(eq? entry root-entry) entry]
                      [(null? (file-entries-layout-items entry)) entry]
                      [(fx< (found-file-members-passed f) member-count)
                       (for-each
                         (lambda (it)
                           (set! notes
                             (cons (format "~a: ~a is ignored, because this file belongs to a submodule of the project at ~a, whose own .chez-tools sets the library layout"
                                     (file-entries-path entry) (entry-name it) (file-entries-dir root-entry))
                                   notes)))
                         (file-entries-layout-items entry))
                       (without-layout-entries entry)]
                      [else
                       (let ([it (car (file-entries-layout-items entry))])
                         (config-error (file-entries-path entry) (item-start it) (item-end it)
                           "~a is accepted only in the project's root .chez-tools, ~a"
                           (entry-name it) (file-entries-path root-entry)))])))
                (let ([checked (map role-checked found)])
                  (merge-entries (reverse checked)
                    (cond
                      [root-entry (file-entries-dir root-entry)]
                      [stop stop]
                      [else (normalise-path fallback-root)])
                    (cond
                      [root-entry (root-marker configuration)]
                      [stop (root-marker git)]
                      [else (root-marker fallback)])
                    (reverse files)
                    (reverse absent)
                    (reverse notes)))))))
      (let loop ([dir (let ([p (canonical-path path)]) (if (file-directory? p) p (path-parent p)))]
                 [found '()] [members '()])
        (let* ([file (in-directory dir config-file-name)]
               [stamp (file-stamp file)]
               [entry (and stamp (decode-file file (read-file-text file)))]
               [found (if entry (cons (make-found-file entry (length members)) found) found)])
          (if stamp (note-file! file stamp) (note-absent! file))
          (cond
            [(and entry (file-entries-root? entry)) (finish found members #f)]
            [(and (repository? dir) (not (member-repository? dir))) (finish found members dir)]
            [(string=? dir "/") (finish found members #f)]
            [else (loop (path-parent dir) found (if (repository? dir) (append members (list dir)) members))])))))

  (define-record-type found-file
    (nongenerative) (sealed #t)
    (fields entries members-passed))

  (define (without-layout-entries entry)
    (make-file-entries (file-entries-path entry) (file-entries-dir entry) (file-entries-root? entry)
      (filter (lambda (single) (not (memq (car single) layout-keys))) (file-entries-singles entry))
      '() (file-entries-file-kinds entry) (file-entries-reader-modes entry) (file-entries-worker entry)
      (file-entries-lint entry) (file-entries-format entry) (file-entries-environment entry)))

  (define (first-rule rules path)
    (cond
      [(find (lambda (r) (glob-match? (car r) path)) rules) => cdr]
      [else #f]))

  (define (config-file-kind c path) (first-rule (config-file-kinds c) path))

  (define (config-reader-mode c path) (first-rule (config-reader-modes c) path))

  (define (config-excluded? c path)
    (exists (lambda (g) (glob-match? g path)) (config-excludes c)))

  ;; A directory below the named one that cannot be read is named in its
  ;; project's layout notes, so it is skipped here; hidden directories hold
  ;; version control and tools' state, not the project's sources.
  (define (config-source-files c dir)
    (unless (config? c) (assertion-violationf 'config-source-files "~s is not a configuration" c))
    (unless (string? dir) (assertion-violationf 'config-source-files "~s is not a directory" dir))
    (let walk ([dir dir] [top? #t])
      (apply append
        (map (lambda (name)
               (let ([p (string-append dir "/" name)])
                 (cond
                   [(config-excluded? c p) '()]
                   ;; A link can lead out of the tree, or back into it for ever.
                   [(file-symbolic-link? p) '()]
                   [(file-directory? p) (if (char=? (string-ref name 0) #\.) '() (walk p #f))]
                   [(member (string-append "." (or (path-extension p) "")) (config-source-extensions c)) (list p)]
                   [else '()])))
             (list-sort string<?
               (if top? (directory-list dir) (guard (e [(i/o-error? e) '()]) (directory-list dir)))))))))
