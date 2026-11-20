(library (analysis reader)
  (export
    ;; modes
    make-reader-mode reader-mode? reader-mode-syntax reader-mode-fold
    syntax-mode syntax-mode-set fold-mode fold-mode-set
    ;; reading
    read-cst cst? cst-text cst-root cst-tokens cst-diagnostics cst-mode-at
    ;; tokens and nodes
    syntax-kind syntax-kind-set syntax-subkind syntax-subkind-set
    token? token-kind token-subkind token-start token-end token-flags
    token-flag token-flag-set
    token-text symbol-token-name string-token-value
    node? node-kind node-start node-end node-children node-data node-has-error?
    node-head-token
    ;; queries
    cst-token-at cst-path-at cst-node-at-span cst-cursor-context cst-quoted-data? cst-syntax-template?
    cursor-context? cursor-context-region cursor-context-prefix-start cursor-context-open-lists
    cst-top-level-forms top-level-form? top-level-form-node top-level-form-mode
    ;; R6RS library headers
    cst-library-header cst-import-specs cst-file-header
    library-header? library-header-name library-header-name-node library-header-exports library-header-imports
    export-spec? export-spec-internal export-spec-external
    import-spec? import-spec-node import-spec-library-name import-spec-library-node
    import-spec-identifiers import-spec-listed-names import-spec-renames)
  (import (chezscheme) (analysis text) (analysis diagnostic))

  (define-enumeration syntax-mode (r6rs chezscheme) syntax-mode-set)
  (define-enumeration fold-mode (fold no-fold) fold-mode-set)

  (define-enumeration syntax-kind
    (whitespace line-comment block-comment directive script-header unread-tail
     open close dot prefix
     symbol number string character boolean gensym primitive special reference
     error
     root list abbreviation datum-comment)
    syntax-kind-set)

  (define-enumeration syntax-subkind
    (paren bracket vector sized-vector bytevector sized-bytevector fxvector sized-fxvector
     flvector sized-flvector stencil-vector sized-stencil-vector record
     quote quasiquote unquote unquote-splicing syntax quasisyntax unsyntax unsyntax-splicing
     box datum-comment mark
     eof bwp base-rtd
     r6rs chezscheme fold-case no-fold-case
     level-2 level-3
     fasl)
    syntax-subkind-set)

  (define-enumeration token-flag (unterminated unmatched) token-flag-set)

  (define no-flags (token-flag-set))
  (define unterminated-flags (token-flag-set unterminated))
  (define unmatched-flags (token-flag-set unmatched))

  (define-record-type reader-mode
    (nongenerative) (sealed #t)
    (fields syntax fold)
    (protocol
      (lambda (new)
        (lambda (syntax-name fold)
          (unless (memq syntax-name '(r6rs chezscheme))
            (assertion-violationf 'make-reader-mode "~s is not a syntax mode" syntax-name))
          (unless (memq fold '(fold no-fold))
            (assertion-violationf 'make-reader-mode "~s is not a fold mode" fold))
          (new syntax-name fold)))))

  (define reader-modes
    (list (make-reader-mode 'r6rs 'fold) (make-reader-mode 'r6rs 'no-fold)
          (make-reader-mode 'chezscheme 'fold) (make-reader-mode 'chezscheme 'no-fold)))

  (define (canonical-reader-mode syntax-name fold)
    (find (lambda (m) (and (eq? (reader-mode-syntax m) syntax-name) (eq? (reader-mode-fold m) fold)))
          reader-modes))

  (define-record-type token
    (nongenerative) (sealed #t)
    (fields kind subkind start end flags))

  (define-record-type node
    (nongenerative) (sealed #t)
    (fields kind start end children has-error?))

  (define-record-type top-level-form
    (nongenerative) (sealed #t)
    (fields node mode))

  (define-record-type cst
    (nongenerative) (sealed #t)
    (fields text root tokens diagnostics transitions forms))

  (define (datum-start x) (if (token? x) (token-start x) (node-start x)))
  (define (datum-end x) (if (token? x) (token-end x) (node-end x)))

  (define (datum-token? tok)
    (memq (token-kind tok)
      '(symbol number string character boolean gensym primitive special reference error)))

  (define (node-data x)
    (unless (node? x) (assertion-violationf 'node-data "~s is not a node" x))
    (let ([children (node-children x)])
      (let loop ([i (fx- (vector-length children) 1)] [acc '()])
        (if (fx< i 0)
            (list->vector acc)
            (let ([c (vector-ref children i)])
              (loop (fx- i 1)
                    (if (if (token? c) (datum-token? c) (memq (node-kind c) '(list abbreviation)))
                        (cons c acc)
                        acc)))))))

  (define (node-head-token x)
    (unless (node? x) (assertion-violationf 'node-head-token "~s is not a node" x))
    (and (eq? (node-kind x) 'list)
         (let ([data (node-data x)])
           (and (fx> (vector-length data) 0)
                (token? (vector-ref data 0))
                (vector-ref data 0)))))

  (include "analysis/reader/number.ss")
  (include "analysis/reader/lexer.ss")
  (include "analysis/reader/parser.ss")

  (define (read-cst t mode)
    (unless (text? t) (assertion-violationf 'read-cst "~s is not a text" t))
    (unless (reader-mode? mode) (assertion-violationf 'read-cst "~s is not a reader mode" mode))
    (let ([lx (new-lexer (text-string t) (eq? (reader-mode-syntax mode) 'r6rs)
                (eq? (reader-mode-fold mode) 'fold))])
      (parse-text t lx (canonical-reader-mode (reader-mode-syntax mode) (reader-mode-fold mode)))))

  (define (check-cst who x)
    (unless (cst? x) (assertion-violationf who "~s is not a CST" x)))

  (define (check-offset who c offset)
    (unless (and (fixnum? offset) (fx<= 0 offset (text-length (cst-text c))))
      (assertion-violationf who "~s is not an offset in the CST's text" offset)))

  (define (check-token who c tok)
    (unless (token? tok) (assertion-violationf who "~s is not a token" tok))
    (unless (fx<= (token-end tok) (text-length (cst-text c)))
      (assertion-violationf who "~s is not a token of this CST" tok)))

  (define (cst-mode-at c offset)
    (check-cst 'cst-mode-at c)
    (check-offset 'cst-mode-at c offset)
    (mode-at c offset))

  (define (mode-at c offset)
    (let ([v (cst-transitions c)])
      (let loop ([lo 0] [hi (vector-length v)])
        (if (fx= (fx+ lo 1) hi)
            (cdr (vector-ref v lo))
            (let ([mid (fxsrl (fx+ lo hi) 1)])
              (if (fx<= (car (vector-ref v mid)) offset)
                  (loop mid hi)
                  (loop lo mid)))))))

  (define (token-text c tok)
    (check-cst 'token-text c)
    (check-token 'token-text c tok)
    (text-substring (cst-text c) (token-start tok) (token-end tok)))

  (define (symbol-token-name c tok)
    (check-cst 'symbol-token-name c)
    (check-token 'symbol-token-name c tok)
    (token-name c tok))

  (define (string-token-value c tok)
    (check-cst 'string-token-value c)
    (check-token 'string-token-value c tok)
    (unless (eq? (token-kind tok) 'string) (assertion-violationf 'string-token-value "~s is not a string token" tok))
    (decode-string-token (text-string (cst-text c)) (token-start tok)
      (eq? (reader-mode-syntax (mode-at c (token-start tok))) 'r6rs)))

  (define (token-name c tok)
    (and (memq (token-kind tok) '(symbol gensym primitive))
         (let ([mode (mode-at c (token-start tok))])
           (let-values ([(name slashed?)
                         (decode-token-name (text-string (cst-text c)) (token-start tok)
                           (eq? (reader-mode-syntax mode) 'r6rs) (eq? (reader-mode-fold mode) 'fold))])
             (if (and (eq? (reader-mode-fold mode) 'fold) (not slashed?))
                 (string-foldcase name)
                 name)))))

  (define (last-starting-at-or-before v offset)
    (let loop ([lo -1] [hi (vector-length v)])
      (if (fx= (fx+ lo 1) hi)
          lo
          (let ([mid (fxsrl (fx+ lo hi) 1)])
            (if (fx<= (datum-start (vector-ref v mid)) offset)
                (loop mid hi)
                (loop lo mid))))))

  (define (cst-token-at c offset)
    (check-cst 'cst-token-at c)
    (check-offset 'cst-token-at c offset)
    (let* ([v (cst-tokens c)] [i (last-starting-at-or-before v offset)])
      (and (fx>= i 0)
           (let ([tok (vector-ref v i)])
             (and (fx< offset (token-end tok)) tok)))))

  (define (child-containing x offset)
    (let* ([children (node-children x)] [i (last-starting-at-or-before children offset)])
      (and (fx>= i 0)
           (let ([child (vector-ref children i)])
             (and (fx< offset (datum-end child)) child)))))

  (define (cst-path-at c offset)
    (check-cst 'cst-path-at c)
    (check-offset 'cst-path-at c offset)
    (let loop ([x (cst-root c)] [path '()])
      (let ([child (child-containing x offset)])
        (if (and child (node? child))
            (loop child (cons x path))
            (reverse (cons x path))))))

  ;; Quoted data is under ' or under ` and not under , or ,@; a token
  ;; under #' or #` is a syntax template, which can name a binding, and
  ;; is code again under #, or #,@.
  (define (cst-quoted-data? c tok)
    (check-cst 'cst-quoted-data? c)
    (unless (token? tok) (assertion-violationf 'cst-quoted-data? "~s is not a token" tok))
    (and (memq (datum-state c tok) '(quote quasiquote)) #t))

  (define (cst-syntax-template? c tok)
    (check-cst 'cst-syntax-template? c)
    (unless (token? tok) (assertion-violationf 'cst-syntax-template? "~s is not a token" tok))
    (eq? (datum-state c tok) 'template))

  (define (datum-state c tok)
    (let loop ([path (cst-path-at c (token-start tok))] [state #f])
      (if (null? path)
          state
          (let ([n (car path)])
            (loop (cdr path)
                  (if (and (node? n) (eq? (node-kind n) 'abbreviation))
                      (case (token-subkind (vector-ref (node-children n) 0))
                        [(quote) (if (eq? state 'quasiquote) state 'quote)]
                        [(quasiquote) (if (eq? state 'quote) state 'quasiquote)]
                        [(unquote unquote-splicing) (if (eq? state 'quasiquote) #f state)]
                        [(syntax quasisyntax) (if (memq state '(quote quasiquote)) state 'template)]
                        [(unsyntax unsyntax-splicing) (if (eq? state 'template) #f state)]
                        [else state])
                      state))))))

  (define (datum? x)
    (if (token? x) (datum-token? x) (memq (node-kind x) '(list abbreviation))))

  (define (cst-node-at-span c start end)
    (check-cst 'cst-node-at-span c)
    (check-offset 'cst-node-at-span c start)
    (check-offset 'cst-node-at-span c end)
    (let loop ([x (cst-root c)])
      (let ([child (child-containing x start)])
        (cond
          [(not child) #f]
          [(and (fx= (datum-start child) start) (fx= (datum-end child) end) (datum? child)) child]
          [(and (node? child) (fx<= end (node-end child))) (loop child)]
          [else #f]))))

  (define-record-type cursor-context
    (nongenerative) (sealed #t)
    (fields region prefix-start open-lists))

  (define (closer-of x)
    (let* ([children (node-children x)] [last (vector-ref children (fx- (vector-length children) 1))])
      (and (token? last) (eq? (token-kind last) 'close) (fx> (vector-length children) 1) last)))

  (define (cst-cursor-context c offset)
    (check-cst 'cst-cursor-context c)
    (check-offset 'cst-cursor-context c offset)
    (let* ([tok (and (fx> offset 0) (cst-token-at c (fx- offset 1)))]
           [unterminated? (and tok (enum-set-member? 'unterminated (token-flags tok)))]
           [inside? (and tok (or (fx< offset (token-end tok)) unterminated?))]
           [open-lists
            (let loop ([x (cst-root c)] [acc '()])
              (let* ([children (node-children x)]
                     [i (last-starting-at-or-before children (fx- offset 1))]
                     [child (and (fx>= i 0) (vector-ref children i))])
                (if (and child (node? child) (fx< (node-start child) offset)
                         (or (fx< offset (node-end child))
                             (and (fx= offset (node-end child)) (not (closer-of child))
                                  (memq (node-kind child) '(list abbreviation datum-comment)))))
                    (loop child
                          (if (and (eq? (node-kind child) 'list)
                                   (fx<= (token-end (vector-ref (node-children child) 0)) offset)
                                   (let ([closer (closer-of child)])
                                     (or (not closer) (fx<= offset (token-start closer)))))
                              (cons child acc)
                              acc))
                    acc)))]
           [in-datum-comment?
            (let loop ([x (cst-root c)])
              (let* ([children (node-children x)]
                     [i (last-starting-at-or-before children (fx- offset 1))]
                     [child (and (fx>= i 0) (vector-ref children i))])
                (and child (node? child) (fx< (node-start child) offset) (fx<= offset (node-end child))
                     (or (and (eq? (node-kind child) 'datum-comment)
                              (fx<= (token-end (vector-ref (node-children child) 0)) offset))
                         (loop child)))))]
           [region
            (cond
              [(not tok) (if in-datum-comment? 'datum-comment 'code)]
              [(and (eq? (token-kind tok) 'string) inside?) 'string]
              [(eq? (token-kind tok) 'line-comment) 'comment]
              [(and (memq (token-kind tok) '(block-comment script-header)) inside?) 'comment]
              [(eq? (token-kind tok) 'unread-tail) 'comment]
              [(or (eq? (token-kind tok) 'character)
                   (and (eq? (token-kind tok) 'error)
                        (string-prefix? "#\\" (text-substring (cst-text c) (token-start tok) (token-end tok)))))
               'character]
              [in-datum-comment? 'datum-comment]
              [else 'code])])
      (make-cursor-context region
        (if (and tok (eq? (token-kind tok) 'symbol) (memq region '(code datum-comment)))
            (token-start tok)
            offset)
        open-lists)))

  (define (string-prefix? prefix s)
    (and (fx<= (string-length prefix) (string-length s))
         (string=? prefix (substring s 0 (string-length prefix)))))

  (define (cst-top-level-forms c)
    (check-cst 'cst-top-level-forms c)
    (cst-forms c))

  (define-record-type library-header
    (nongenerative) (sealed #t)
    (fields name name-node exports imports))

  (define-record-type export-spec
    (nongenerative) (sealed #t)
    (fields internal external))

  (define-record-type import-spec
    (nongenerative) (sealed #t)
    (fields node library-name library-node identifiers listed-names renames))

  ;; R6RS 7.1 fixes the header's grammar, and library is valid only at top
  ;; level, where it cannot be shadowed, so reading the header by the symbol
  ;; library is exact.
  (define (header-readers c)
    (define (identifier-token? x) (and (token? x) (eq? (token-kind x) 'symbol)))
    (define (named? x name) (and (identifier-token? x) (string=? (token-name c x) name)))
    (define (list-form? x)
      (and (node? x) (eq? (node-kind x) 'list)
           (memq (token-subkind (vector-ref (node-children x) 0)) '(paren bracket))
           (not (exists (lambda (child) (and (token? child) (eq? (token-kind child) 'dot)))
                        (vector->list (node-children x))))))
    (define (proper-list x)
      (and (list-form? x) (closer-of x) (vector->list (node-data x))))
    (define (exact-integer x)
      (and (token? x) (eq? (token-kind x) 'number)
           (let ([v (number-token-value (text-string (cst-text c)) x
                      (eq? (reader-mode-syntax (mode-at c (token-start x))) 'r6rs))])
             (and (integer? v) (exact? v) v))))
    (define (sub-version? x) (let ([v (exact-integer x)]) (and v (>= v 0))))
    (define (one-item items) (and (pair? items) (null? (cdr items)) (car items)))
    (define (library-name x version-ok?)
      (let ([items (proper-list x)])
        (and items
             (pair? items)
             (identifier-token? (car items))
             (let loop ([items items] [names '()])
               (cond
                 [(null? items) (reverse names)]
                 [(identifier-token? (car items)) (loop (cdr items) (cons (token-name c (car items)) names))]
                 [(and (null? (cdr items)) (version-ok? (car items))) (reverse names)]
                 [else #f])))))
    (define (version? x)
      (let ([items (proper-list x)]) (and items (andmap sub-version? items))))
    (define (sub-version-reference? x)
      (or (sub-version? x)
          (let ([items (proper-list x)])
            (and items (pair? items)
                 (let ([head (car items)] [rest (cdr items)])
                   (cond
                     [(or (named? head ">=") (named? head "<=")) (let ([y (one-item rest)]) (and y (sub-version? y)))]
                     [(or (named? head "and") (named? head "or")) (andmap sub-version-reference? rest)]
                     [(named? head "not") (let ([y (one-item rest)]) (and y (sub-version-reference? y)))]
                     [else #f]))))))
    (define (version-reference? x)
      (let ([items (proper-list x)])
        (and items
             (cond
               [(and (pair? items) (or (named? (car items) "and") (named? (car items) "or")))
                (andmap version-reference? (cdr items))]
               [(and (pair? items) (named? (car items) "not"))
                (let ([y (one-item (cdr items))]) (and y (version-reference? y)))]
               [else (andmap sub-version-reference? items)]))))
    (define (import-level? x)
      (or (named? x "run") (named? x "expand")
          (let ([items (proper-list x)])
            (and items (fx= (length items) 2) (named? (car items) "meta") (exact-integer (cadr items)) #t))))
    (define (export-specs x)
      (let ([items (proper-list x)])
        (and items (pair? items) (named? (car items) "export")
             (let loop ([items (cdr items)] [acc '()])
               (cond
                 [(null? items) (reverse acc)]
                 [(identifier-token? (car items))
                  (loop (cdr items) (cons (make-export-spec (car items) (car items)) acc))]
                 [(let ([r (proper-list (car items))]) (and r (pair? r) (named? (car r) "rename") (cdr r)))
                  => (lambda (pairs)
                       (let ([specs (map (lambda (p)
                                           (let ([ids (proper-list p)])
                                             (and ids (fx= (length ids) 2) (andmap identifier-token? ids)
                                                  (make-export-spec (car ids) (cadr ids)))))
                                         pairs)])
                         (and (andmap values specs) (loop (cdr items) (append (reverse specs) acc)))))]
                 [else #f])))))
    (define (import-set x)
      (define (fail) (values #f #f #f '() '()))
      (define (reference x)
        (let ([name (library-name x version-reference?)])
          (if name (values name x '() '() '()) (fail))))
      (let ([items (proper-list x)])
        (if (not (and items (pair? items)))
            (fail)
            (let ([head (car items)] [rest (cdr items)])
              (define (with-set ids listed renames)
                (if (and ids (pair? rest))
                    (let-values ([(name node inner-ids inner-listed inner-renames) (import-set (car rest))])
                      (if name
                          (values name node (append inner-ids ids) (append inner-listed listed) (append inner-renames renames))
                          (fail)))
                    (fail)))
              (define (pairs ps)
                (let ([ids (map proper-list ps)])
                  (and (andmap (lambda (p) (and p (fx= (length p) 2) (andmap identifier-token? p))) ids)
                       (map (lambda (p) (cons (car p) (cadr p))) ids))))
              (cond
                [(or (named? head "only") (named? head "except"))
                 (let ([ids (and (pair? rest) (andmap identifier-token? (cdr rest)) (cdr rest))])
                   (with-set ids ids '()))]
                [(named? head "prefix")
                 (with-set (and (pair? rest) (pair? (cdr rest)) (null? (cddr rest))
                                (identifier-token? (cadr rest)) (cdr rest))
                   '() '())]
                [(named? head "rename")
                 (let ([renames (and (pair? rest) (pairs (cdr rest)))])
                   (with-set (and renames (apply append (map (lambda (p) (list (car p) (cdr p))) renames)))
                     (if renames (map car renames) '()) (or renames '())))]
                [(named? head "library")
                 (if (and (pair? rest) (null? (cdr rest))) (reference (car rest)) (fail))]
                [else (reference x)])))))
    (define (import-spec x)
      (let* ([items (proper-list x)]
             [set (if (and items (pair? items) (named? (car items) "for") (pair? (cdr items)))
                      (and (andmap import-level? (cddr items)) (cadr items))
                      x)])
        (and set
             (let-values ([(name node ids listed renames) (import-set set)])
               (and name (make-import-spec x name node ids listed renames))))))
    (define (import-specs x)
      (let ([items (proper-list x)])
        (and items (pair? items) (named? (car items) "import")
             (let ([specs (map import-spec (cdr items))])
               (and (andmap values specs) specs)))))
    ;; The library form itself may be unclosed, so that its imports stay
    ;; known while its body is being written.
    (define (library-header x)
      (and (list-form? x)
           (let ([data (vector->list (node-data x))])
             (and (fx>= (length data) 4)
                  (named? (car data) "library")
                  (let ([name (library-name (cadr data) version?)]
                        [exports (export-specs (caddr data))]
                        [imports (import-specs (cadddr data))])
                    (and name exports imports
                         (make-library-header name (cadr data) exports imports)))))))
    (values library-header import-specs))

  (define (cst-library-header c x)
    (check-cst 'cst-library-header c)
    (unless (or (node? x) (token? x))
      (assertion-violationf 'cst-library-header "~s is not a node or a token" x))
    (let-values ([(library-header import-specs) (header-readers c)])
      (library-header x)))

  ;; An import form of a program or a script has the grammar of a library
  ;; header's import clause.
  (define (cst-import-specs c x)
    (check-cst 'cst-import-specs c)
    (unless (or (node? x) (token? x))
      (assertion-violationf 'cst-import-specs "~s is not a node or a token" x))
    (let-values ([(library-header import-specs) (header-readers c)])
      (import-specs x)))

  (define (cst-file-header c)
    (check-cst 'cst-file-header c)
    (let ([forms (cst-forms c)])
      (if (fx= (vector-length forms) 0)
          (values #f '())
          (let ([x (top-level-form-node (vector-ref forms 0))])
            (let-values ([(library-header import-specs) (header-readers c)])
              (cond
                [(library-header x) => (lambda (h) (values h (library-header-imports h)))]
                [(import-specs x) => (lambda (specs) (values #f specs))]
                [else (values #f '())])))))))
