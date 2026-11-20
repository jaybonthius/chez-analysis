(import (chezscheme) (check) (analysis text) (analysis reader) (analysis diagnostic))

(define chez-mode (make-reader-mode 'chezscheme 'no-fold))
(define r6rs-mode (make-reader-mode 'r6rs 'no-fold))

(define (read-text s) (read-cst (make-text s) chez-mode))
(define (read-r6rs s) (read-cst (make-text s) r6rs-mode))

(define (tokens-of c)
  (map (lambda (t) (list (token-kind t) (token-subkind t) (token-text c t)))
       (vector->list (cst-tokens c))))

(define (significant-tokens s)
  (filter (lambda (t) (not (eq? (car t) 'whitespace))) (tokens-of (read-text s))))

(define (diagnostics-of c)
  (map (lambda (d) (list (diagnostic-start d) (diagnostic-end d) (diagnostic-message d)))
       (vector->list (cst-diagnostics c))))

(define (flags-of t) (enum-set->list (token-flags t)))

(define (first-token c kind)
  (find (lambda (t) (eq? (token-kind t) kind)) (vector->list (cst-tokens c))))

(define (all-nodes c)
  (let loop ([x (cst-root c)] [acc '()])
    (if (node? x)
        (fold-left (lambda (acc child) (loop child acc)) (cons x acc) (vector->list (node-children x)))
        acc)))

(define (node-texts c x)
  (map (lambda (d) (if (token? d) (token-text c d) (text-substring (cst-text c) (node-start d) (node-end d))))
       (vector->list (node-data x))))

(test "whitespace is one token per maximal run"
  (check-equal? '((whitespace #f " \t\n\x2028;") (symbol #f "a") (whitespace #f "\x85;"))
    (tokens-of (read-text " \t\n\x2028;a\x85;"))))

(test "a line comment ends before LF, CR, NEL and LS, not before PS"
  (check-equal? '((line-comment #f "; a") (whitespace #f "\n") (symbol #f "b"))
    (tokens-of (read-text "; a\nb")))
  (check-equal? '((line-comment #f "; a\x2029;b")) (tokens-of (read-text "; a\x2029;b")))
  (check-equal? '(line-comment #f "; a") (car (tokens-of (read-text "; a\x2028;b")))))

(test "block comments nest, and an unterminated one runs to the end"
  (check-equal? '((block-comment #f "#| a #| b |# c |#") (symbol #f "d"))
    (tokens-of (read-text "#| a #| b |# c |#d")))
  (let ([c (read-text "#| a")])
    (check-equal? '(unterminated) (flags-of (first-token c 'block-comment)))
    (check-equal? '((0 4 "unexpected end-of-file reading block comment")) (diagnostics-of c))))

(test "the four directives need no delimiter and change the mode after them"
  (check-equal? '((directive r6rs "#!r6rs") (symbol #f "foo")) (tokens-of (read-text "#!r6rsfoo")))
  (check-equal? '((directive chezscheme "#!chezscheme") (directive fold-case "#!fold-case")
                  (directive no-fold-case "#!no-fold-case"))
    (tokens-of (read-text "#!chezscheme#!fold-case#!no-fold-case")))
  (let ([c (read-text "a #!r6rs b #!fold-case c")])
    (check-equal? 'chezscheme (reader-mode-syntax (cst-mode-at c 0)))
    (check-equal? 'chezscheme (reader-mode-syntax (cst-mode-at c 5)))
    (check-equal? 'r6rs (reader-mode-syntax (cst-mode-at c 8)))
    (check-equal? 'no-fold (reader-mode-fold (cst-mode-at c 10)))
    (check-equal? 'fold (reader-mode-fold (cst-mode-at c 23)))))

(test "a script header is a token only at the start"
  (check-equal? '((script-header #f "#! /bin/sh\n") (symbol #f "a")) (tokens-of (read-text "#! /bin/sh\na")))
  (check-equal? '((script-header #f "#!/usr/bin/env scheme")) (tokens-of (read-text "#!/usr/bin/env scheme")))
  (check-equal? '((0 3 "invalid syntax #!x"))
    (diagnostics-of (read-text "#!x y")))
  (check-equal? '((2 4 "invalid syntax #!"))
    (diagnostics-of (read-text "a #! /bin/sh"))))

(test "text after a top-level #!eof is one unread tail"
  (check-equal? '((symbol #f "a") (whitespace #f " ") (special eof "#!eof") (unread-tail #f " (b \"")) 
    (tokens-of (read-text "a #!eof (b \"")))
  (check-equal? '() (diagnostics-of (read-text "a #!eof (b \"")))
  (check-equal? '(list) (map node-kind (filter node? (vector->list (node-children (cst-root (read-text "(a #!eof b)")))))))
  (check-equal? 1 (vector-length (cst-top-level-forms (read-text "(a #!eof b)")))))

(test "openers of every subkind"
  (check-equal?
    '((open paren "(") (open bracket "[") (open vector "#(") (open sized-vector "#3(")
      (open bytevector "#vu8(") (open sized-bytevector "#2vu8(") (open fxvector "#vfx(")
      (open sized-fxvector "#2vfx(") (open flvector "#vfl(") (open sized-flvector "#2vfl(")
      (open stencil-vector "#vs(") (open sized-stencil-vector "#5vs(") (open record "#["))
    (filter (lambda (t) (eq? (car t) 'open))
      (tokens-of (read-text "( [ #( #3( #vu8( #2vu8( #vfx( #2vfx( #vfl( #2vfl( #vs( #5vs( #[")))))

(test "closers, and a closer with no opener is unmatched"
  (let ([c (read-text "(a] ) ]")])
    (check-equal? '((close bracket "]") (close paren ")") (close bracket "]"))
      (filter (lambda (t) (eq? (car t) 'close)) (tokens-of c)))
    (check-equal? '(() (unmatched) (unmatched))
      (map flags-of (filter (lambda (t) (eq? (token-kind t) 'close)) (vector->list (cst-tokens c)))))
    (check-equal? '((0 3 "parenthesized list terminated by bracket")
                    (4 5 "unexpected close parenthesis")
                    (6 7 "unexpected close bracket"))
      (diagnostics-of c))))

(test "a dot is a token only where it stands alone"
  (check-equal? '((open paren "(") (symbol #f "a") (dot #f ".") (symbol #f "b") (close paren ")"))
    (significant-tokens "(a . b)"))
  (check-equal? '((symbol #f "a.b") (symbol #f ".b") (symbol #f "..") (symbol #f "...") (number #f ".5"))
    (significant-tokens "a.b .b .. ... .5")))

(test "prefixes of every subkind"
  (check-equal?
    '((prefix quote "'") (prefix quasiquote "`") (prefix unquote ",") (prefix unquote-splicing ",@")
      (prefix syntax "#'") (prefix quasisyntax "#`") (prefix unsyntax "#,") (prefix unsyntax-splicing "#,@")
      (prefix box "#&") (prefix datum-comment "#;") (prefix mark "#12="))
    (filter (lambda (t) (eq? (car t) 'prefix))
      (tokens-of (read-text "'a `a ,a ,@a #'a #`a #,a #,@a #&a #;a #12=a")))))

(test "symbols, with Chez's extensions"
  (check-equal?
    '((symbol #f "abc") (symbol #f "->x") (symbol #f "+") (symbol #f "-") (symbol #f "1+")
      (symbol #f "a|b c|d") (symbol #f "\\x41;b") (symbol #f "\\#x") (symbol #f "{") (symbol #f "}")
      (symbol #f "λ") (symbol #f "@if") (symbol #f "3#f") (symbol #f "1|a"))
    (significant-tokens "abc ->x + - 1+ a|b c|d \\x41;b \\#x { } λ @if 3#f 1|a"))
  (check-equal? '((symbol #f "a") (prefix quote "'") (symbol #f "b")) (significant-tokens "a'b"))
  (let ([c (read-text "|abc")])
    (check-equal? '(unterminated) (flags-of (first-token c 'symbol)))
    (check-equal? '((0 4 "unexpected end-of-file reading symbol")) (diagnostics-of c))))

(test "a number-like token is a number only when it parses as one"
  (check-equal?
    '((number #f "12") (number #f "-1.5e3") (number #f "#x1F") (number #f "#36rzz") (number #f "1/2")
      (number #f "+inf.0") (number #f "-i") (number #f "#e1.5") (number #f "1@2") (symbol #f "1a"))
    (significant-tokens "12 -1.5e3 #x1F #36rzz 1/2 +inf.0 -i #e1.5 1@2 1a"))
  (check-equal? '((0 3 "cannot represent 1/0")) (diagnostics-of (read-text "1/0")))
  (check-equal? '((0 4 "invalid number syntax #xzz")) (diagnostics-of (read-text "#xzz")))
  (check-equal? '((0 2 "unexpected end-of-file reading number")) (diagnostics-of (read-text "#x"))))

(test "in #!r6rs mode # ends a number-like token and Chez's extensions are errors"
  (check-equal? '((directive r6rs "#!r6rs") (number #f "3") (boolean #f "#f")) (significant-tokens "#!r6rs 3#f"))
  (check-equal? '((0 5 "1/2e3 number syntax is not allowed in #!r6rs mode")) (diagnostics-of (read-r6rs "1/2e3")))
  (check-equal? '((0 2 "1+ symbol syntax is not allowed in #!r6rs mode")) (diagnostics-of (read-r6rs "1+")))
  (check-equal? '((0 2 "delimiter ' is not allowed in #!r6rs mode")) (diagnostics-of (read-r6rs "a'b")))
  (check-equal? '((0 2 "#% primitive syntax is not allowed in #!r6rs mode")) (diagnostics-of (read-r6rs "#%car")))
  (check-equal? '() (diagnostics-of (read-r6rs "(a [b] #vu8(1) \"\\x41;\")"))))

(test "strings, their escapes and their errors"
  (check-equal? '((string #f "\"a\\n\\x41;\\\n  b\"")) (significant-tokens "\"a\\n\\x41;\\\n  b\""))
  (check-equal? '((3 5 "invalid string character \\q")) (diagnostics-of (read-text "\"ab\\q\"")))
  (check-equal? '((1 4 "invalid character \" in string hex escape")) (diagnostics-of (read-text "\"\\x4\"")))
  (let ([c (read-text "(a \"bc")])
    (check-equal? '(unterminated) (flags-of (first-token c 'string)))
    (check-equal? '(3 6 "unexpected end-of-file reading string") (car (diagnostics-of c)))))

(test "a string's value is decoded as Chez reads it, and an erroneous string has none"
  (for-each
    (lambda (s)
      (let* ([c (read-text s)] [tok (first-token c 'string)])
        (check-equal? (list s (read (open-string-input-port s))) (list s (string-token-value c tok)))))
    '("\"\"" "\"a\\n\\x41;\\\n  b\"" "\"\\a\\b\\t\\n\\v\\f\\r\\\\\\\"\""
      "\"\\101\\'\"" "\"a\rb\r\nc\x85;d\x2028;e\"" "\"\\ \t\r\n\t x\\\r\x85;y\"" "\"\x3BB;\\x1F600;\""))
  (for-each
    (lambda (c) (check-equal? #f (string-token-value c (first-token c 'string))))
    (list (read-text "\"ab\\q\"") (read-text "\"\\x4\"") (read-text "\"abc") (read-r6rs "\"\\101\""))))

(test "characters: names, hex, octal and single characters"
  (check-equal?
    '((character #f "#\\a") (character #f "#\\space") (character #f "#\\x41") (character #f "#\\x")
      (character #f "#\\377") (character #f "#\\(") (character #f "#\\λ") (character #f "#\\nel"))
    (significant-tokens "#\\a #\\space #\\x41 #\\x #\\377 #\\( #\\λ #\\nel"))
  (check-equal? '((0 7 "invalid character name #\\Space")) (diagnostics-of (read-text "#\\Space")))
  (check-equal? '() (diagnostics-of (read-cst (make-text "#\\Space") (make-reader-mode 'chezscheme 'fold))))
  (check-equal? '((0 3 "invalid delimiter y for character")) (diagnostics-of (read-text "#\\xyz")))
  (check-equal? '((0 5 "invalid character #\\400")) (diagnostics-of (read-text "#\\400")))
  (check-equal? '((0 5 "invalid character name #\\nel")) (diagnostics-of (read-r6rs "#\\nel"))))

(test "booleans, short and long"
  (check-equal? '((boolean #f "#t") (boolean #f "#F") (boolean #f "#true") (boolean #f "#False"))
    (significant-tokens "#t #F #true #False"))
  (check-equal? '((0 2 "invalid delimiter 5 for boolean")) (diagnostics-of (read-text "#t5")))
  (check-equal? '((0 2 "invalid boolean #tx")) (diagnostics-of (read-text "#tx")))
  (check-equal? '((0 4 "unexpected end-of-file reading boolean")) (diagnostics-of (read-text "#tru"))))

(test "gensyms in both syntaxes"
  (check-equal? '((gensym #f "#{a b}") (gensym #f "#{a |b c|}") (gensym #f "#:g")) (significant-tokens "#{a b} #{a |b c|} #:g"))
  (check-equal? '((0 5 "expected close brace terminating gensym syntax")) (diagnostics-of (read-text "#{a b c}")))
  (check-equal? '(unterminated) (flags-of (first-token (read-text "#{a b") 'gensym))))

(test "primitives at their three levels"
  (check-equal? '((primitive #f "#%car") (primitive level-2 "#2%car") (primitive level-3 "#3%car"))
    (significant-tokens "#%car #2%car #3%car"))
  (check-equal? '((0 3 "invalid sharp-sign prefix #4%")) (diagnostics-of (read-text "#4%car"))))

(test "the special objects"
  (check-equal? '((open paren "(") (special eof "#!eof") (special bwp "#!bwp") (special base-rtd "#!base-rtd")
                  (close paren ")"))
    (significant-tokens "(#!eof #!bwp #!base-rtd)"))
  (check-equal? '((0 6 "invalid syntax #!void")) (diagnostics-of (read-text "#!void"))))

(test "graph references and marks"
  (check-equal? '((prefix mark "#0=") (open paren "(") (symbol #f "a") (dot #f ".") (reference #f "#0#") (close paren ")"))
    (significant-tokens "#0=(a . #0#)"))
  (check-equal? '() (diagnostics-of (read-text "#0=(a . #0#)")))
  (check-equal? '((0 3 "mark #0= missing")) (diagnostics-of (read-text "#0# #0=a")))
  (check-equal? '((6 9 "duplicate mark #1= seen")) (diagnostics-of (read-text "(#1=a #1=b)")))
  (check-equal? '() (diagnostics-of (read-text "#;#0=a #0#"))))

(test "error tokens keep the text lossless"
  (check-equal? '((error #f "#y") (close paren ")")) (significant-tokens "#y)"))
  (check-equal? '((error #f "#") (whitespace #f " ")) (tokens-of (read-text "# ")))
  (check-equal? '((error fasl "#@")) (significant-tokens "#@"))
  (check-equal? '((0 2 "unsupported old fasl format detected---use new format with binary i/o"))
    (diagnostics-of (read-text "#@"))))

(test "the root holds the top-level data and atmosphere"
  (let ([c (read-text "; c\n(a) b")])
    (check-equal? 'root (node-kind (cst-root c)))
    (check-equal? '(0 9) (list (node-start (cst-root c)) (node-end (cst-root c))))
    (check-equal? '("(a)" "b") (node-texts c (cst-root c)))))

(test "a list holds its opener, children and closer; an unclosed one has no closer"
  (let* ([c (read-text "(a [b] . c)")] [x (vector-ref (node-data (cst-root c)) 0)])
    (check-equal? 'list (node-kind x))
    (check-equal? '("a" "[b]" "c") (node-texts c x))
    (check-equal? 'a (string->symbol (token-text c (node-head-token x)))))
  (let* ([c (read-text "(a (b")] [x (vector-ref (node-data (cst-root c)) 0)])
    (check-equal? '(0 5) (list (node-start x) (node-end x)))
    (check-equal? '("a" "(b") (node-texts c x))
    (check (node-has-error? x))
    (check-equal? '((3 5 "unexpected end-of-file reading list") (0 5 "unexpected end-of-file reading list"))
      (diagnostics-of c))))

(test "an abbreviation holds its prefix, atmosphere and datum"
  (let* ([c (read-text "' ; c\n x")] [x (vector-ref (node-data (cst-root c)) 0)])
    (check-equal? 'abbreviation (node-kind x))
    (check-equal? '(0 8) (list (node-start x) (node-end x)))
    (check-equal? '("x") (node-texts c x)))
  (let* ([c (read-text "(')")] [x (vector-ref (node-data (cst-root c)) 0)])
    (check-equal? '((2 3 "unexpected close parenthesis")) (diagnostics-of c))
    (check-equal? '("'") (node-texts c x))))

(test "a closer after a prefix at top level is reported once"
  (for-each
    (lambda (s)
      (let ([n (string-length s)])
        (check-equal? (list (list (fx- n 1) n "unexpected close parenthesis")) (diagnostics-of (read-text s)))))
    '("')" "#;)" "#0=)" "#&)")))

(test "datum comments nest and are not data"
  (let* ([c (read-text "#; #; a b c")] [root (cst-root c)])
    (check-equal? '("c") (node-texts c root))
    (let ([comment (vector-ref (node-children root) 0)])
      (check-equal? 'datum-comment (node-kind comment))
      (check-equal? '(0 9) (list (node-start comment) (node-end comment)))
      (check (exists (lambda (x) (and (node? x) (eq? (node-kind x) 'datum-comment)))
               (vector->list (node-children comment)))))))

(test "every syntax kind is produced by some text"
  (let* ([c (read-text "#! /bin/sh\n; c\n#| b |# #!r6rs #!chezscheme (a . b) [#t] 1 \"s\" #\\a #{g u} #%car #0=#0# '#;x y #!bwp #y #!eof z")]
         [kinds (append (map token-kind (vector->list (cst-tokens c))) (map node-kind (all-nodes c)))])
    (for-each
      (lambda (kind) (check (memq kind kinds)))
      (enum-set->list (enum-set-universe (syntax-kind-set))))))

(test "every symbolic subkind is in the subkind enumeration, and every member occurs"
  (let* ([c (read-text "( [ #( #3( #vu8( #2vu8( #vfx( #2vfx( #vfl( #2vfl( #vs( #5vs( #[ ) ] 'a `a ,a ,@a #'a #`a #,a #,@a #&a #;a #0=a #!eof #!bwp #!base-rtd #!r6rs #!chezscheme #!fold-case #!no-fold-case #2%a #3%a #@")]
         [subkinds (filter symbol? (map token-subkind (vector->list (cst-tokens c))))]
         [universe (enum-set->list (enum-set-universe (syntax-subkind-set)))])
    (for-each (lambda (k) (check (memq k universe))) subkinds)
    (for-each (lambda (k) (check (memq k subkinds))) universe)))

(test "a CST cannot be changed through its vectors"
  (let ([c (read-text "(a)")])
    (check-raises condition? (vector-set! (node-children (cst-root c)) 0 #f))
    (check-raises condition? (vector-set! (cst-tokens c) 0 #f))))

(test "a symbol's name is decoded as Chez reads it, folded only where the mode folds"
  (let ([c (read-text "a\\x41;b |a b| A\\Bc #{foo bar} #:Pretty #%car 12 \"s\" #!fold-case ABC |ABC| A\\BC #{Foo bar}")])
    (check-equal? '("aAb" "a b" "ABc" "foo" "Pretty" "car" #f #f "abc" "ABC" "ABC" "foo")
      (map (lambda (t) (symbol-token-name c t))
           (filter (lambda (t) (not (memq (token-kind t) '(whitespace directive)))) (vector->list (cst-tokens c))))))
  (let ([c (read-cst (make-text "ABC #!no-fold-case ABC") (make-reader-mode 'chezscheme 'fold))])
    (check-equal? '("abc" "ABC")
      (map (lambda (t) (symbol-token-name c t))
           (filter (lambda (t) (eq? (token-kind t) 'symbol)) (vector->list (cst-tokens c)))))))

(test "diagnostics come in the order Chez would raise them"
  (check-equal? '((5 7 "invalid boolean #tx") (0 9 "parenthesized list terminated by bracket"))
    (diagnostics-of (read-text "(a b #tx]")))
  (check-equal? '((1 2 "unexpected end-of-file reading # prefix") (0 2 "unexpected end-of-file reading list"))
    (diagnostics-of (read-text "(#")))
  (check-equal? '((7 8 "more than one item found after dot (.)")) (diagnostics-of (read-text "(a . b c)")))
  (check-equal? '((5 6 "expected one item after dot (.)")) (diagnostics-of (read-text "(a . )")))
  (check-equal? '((0 8 "too many vector elements supplied")) (diagnostics-of (read-text "#2(a b c)")))
  (check-equal? '((7 10 "invalid value 300 found in bytevector")) (diagnostics-of (read-text "#vu8(1 300)")))
  (check-equal? '((5 6 "non-flonum found in flvector")) (diagnostics-of (read-text "#vfl(1)")))
  (check-equal? '() (diagnostics-of (read-text "#vfl(1.0 +inf.0) #vfx(#e1e2 -3) #vu8(#xff 0)"))))

(test "a span Chez ends past the text ends at the text's end"
  (check-equal? '((0 2 ".. symbol syntax is not allowed in #!r6rs mode")) (diagnostics-of (read-r6rs ".."))))

(test "a missing closer is placed where indentation drops below the earlier siblings"
  (let ([c (read-text "(define (f x)\n  (let ([y 1])\n    (g y))\n(define (h) 2)\n")])
    (check-equal? '((0 55 "unexpected end-of-file reading list")
                    (40 54 "a closing parenthesis or bracket is probably missing before this line"))
      (diagnostics-of c))
    (check-equal? '((read-error error) (missing-closer information))
      (map (lambda (d) (list (diagnostic-rule d) (diagnostic-severity d))) (vector->list (cst-diagnostics c))))))

(test "a missing closer is looked for only in the datum left unclosed"
  (check-equal? '((31 66 "unexpected end-of-file reading list")
                  (51 65 "a closing parenthesis or bracket is probably missing before this line"))
    (diagnostics-of (read-text "(define (f)\n  (let ([x 1])\nx))\n(define (g)\n  (h 1)\n(define (k) 2)\n"))))

(test "the token, path and node at an offset"
  (let ([c (read-text "(a (bc) #0=d)")])
    (check-equal? "bc" (token-text c (cst-token-at c 5)))
    (check-equal? #f (cst-token-at c 13))
    (check-equal? '(root list list) (map node-kind (cst-path-at c 5)))
    (check-equal? '(3 7) (let ([x (cst-node-at-span c 3 7)]) (list (node-start x) (node-end x))))
    (check-equal? 'abbreviation (node-kind (cst-node-at-span c 8 12)))
    (check-equal? "d" (token-text c (cst-node-at-span c 11 12)))
    (check-equal? #f (cst-node-at-span c 2 5))))

(test "the cursor's region, symbol prefix and open lists"
  (let* ([s "(define (f x) \"st ; ing\" ; com\n  #\\sp #;(skip me) (g ab"]
         [c (read-text s)]
         [at (lambda (sub k) (cst-cursor-context c (fx+ (let loop ([i 0]) (if (string=? (substring s i (fx+ i (string-length sub))) sub) i (loop (fx+ i 1)))) k)))])
    (check-equal? 'code (cursor-context-region (at "define" 3)))
    (check-equal? 1 (cursor-context-prefix-start (at "define" 3)))
    (check-equal? 'string (cursor-context-region (at "st ;" 1)))
    (check-equal? 'code (cursor-context-region (at "\" ;" 1)))
    (check-equal? 'comment (cursor-context-region (at "com" 3)))
    (check-equal? 'character (cursor-context-region (at "#\\sp" 3)))
    (check-equal? 'datum-comment (cursor-context-region (at "skip" 2)))
    (let ([context (cst-cursor-context c (string-length s))])
      (check-equal? 'code (cursor-context-region context))
      (check-equal? (fx- (string-length s) 2) (cursor-context-prefix-start context))
      (check-equal? '(2 5) (map (lambda (x) (vector-length (node-data x))) (cursor-context-open-lists context))))
    (check-equal? 1 (length (cursor-context-open-lists (at "(f x)" 5))))
    (check-equal? 2 (length (cursor-context-open-lists (at "(f x)" 4))))))

(test "top-level forms carry the mode at their start"
  (let* ([c (read-text "a #!r6rs (b) #!chezscheme 'c #!eof d")] [forms (vector->list (cst-top-level-forms c))])
    (check-equal? '("a" "(b)" "'c")
      (map (lambda (f) (let ([x (top-level-form-node f)])
                         (if (token? x) (token-text c x) (text-substring (cst-text c) (node-start x) (node-end x)))))
           forms))
    (check-equal? '(chezscheme r6rs chezscheme)
      (map (lambda (f) (reader-mode-syntax (top-level-form-mode f))) forms))))

(define library-text
  "(library (a b (1 2))
     (export x (rename (y z) (w v)))
     (import (rnrs (6)) (only (c) p q) (prefix (except (d) r) d:) (rename (e) (s t))
             (for (f (2 (>= 1) (or 3 (not 4)))) run (meta -1)) (library (only)) (g (and (1) (not ()))))
     (define x 1)")

(test "a library header by R6RS's grammar"
  (let* ([c (read-text library-text)]
         [h (cst-library-header c (vector-ref (node-data (cst-root c)) 0))])
    (check (library-header? h))
    (check-equal? '("a" "b") (library-header-name h))
    (check-equal? "(a b (1 2))" (let ([x (library-header-name-node h)])
                                  (text-substring (cst-text c) (node-start x) (node-end x))))
    (check-equal? '(("x" "x") ("y" "z") ("w" "v"))
      (map (lambda (e) (list (token-text c (export-spec-internal e)) (token-text c (export-spec-external e))))
           (library-header-exports h)))
    (check-equal? '(("rnrs") ("c") ("d") ("e") ("f") ("only") ("g"))
      (map import-spec-library-name (library-header-imports h)))
    (check-equal? '(() ("p" "q") ("r" "d:") ("s" "t") () () ())
      (map (lambda (i) (map (lambda (t) (token-text c t)) (import-spec-identifiers i)))
           (library-header-imports h)))
    (let ([names (lambda (i) (list (map (lambda (t) (token-text c t)) (import-spec-listed-names i))
                                   (map (lambda (p) (list (token-text c (car p)) (token-text c (cdr p)))) (import-spec-renames i))))])
      (check-equal? '((() ()) (("p" "q") ()) (("r") ()) (("s") (("s" "t"))) (() ()) (() ()) (() ()))
        (map names (library-header-imports h))))))

(test "a file's header is its library form or the import form it starts with"
  (let-values ([(h specs) (cst-file-header (read-text library-text))])
    (check-equal? '("a" "b") (library-header-name h))
    (check-equal? 7 (length specs)))
  (let-values ([(h specs) (cst-file-header (read-text "(import (chezscheme) (rename (only (a) f) (f g)))\n(g)"))])
    (check-equal? #f h)
    (check-equal? '(("chezscheme") ("a")) (map import-spec-library-name specs)))
  (let-values ([(h specs) (cst-file-header (read-text "(define x 1)\n(import (a))"))])
    (check-equal? '(#f ()) (list h specs))))

(test "malformed headers and other forms have no header"
  (for-each
    (lambda (s)
      (let ([c (read-text s)])
        (check-equal? #f (cst-library-header c (vector-ref (node-data (cst-root c)) 0)))))
    '("(library (a) (export x) (import (only)))"
      "(library (a) (export 1) (import))"
      "(library a (export) (import))"
      "(library (a) (export) (import (b . c)))"
      "(library (a) (export) (import (alias (b) (c d))))"
      "(library (a) (export) (import (add-prefix (b) c:)))"
      "(library (a) (export) (import (drop-prefix (b) c:)))"
      "(library (a) (export) (import (for (b) bogus)))"
      "(library (a) (export) (import (for (b) (meta 1.5))))"
      "(library (a) (export) (import (b (1 x))))"
      "(library (a) (export) (import (b (>= 1 2))))"
      "(library (a (-1)) (export) (import))"
      "#(library (a) (export) (import (b)))"
      "(library (a) (export) (import (b)) . x)"
      "(define (a) 1)"))
  (let ([c (read-text "(library (a) (export) (import (b))")])
    (check (library-header? (cst-library-header c (vector-ref (node-data (cst-root c)) 0))))))

(test "reading never raises and keeps every character"
  (random-seed 7)
  (let ([alphabet (string->list "()[]{}\";#|\\'`,@.+-019axiefvtu!=%:&λ\x1F600; \n\r\t")])
    (do ([k 0 (fx+ k 1)]) ((fx= k 20000))
      (let* ([s (list->string (map (lambda (i) (list-ref alphabet (random (length alphabet)))) (iota (random 30))))]
             [c (read-text s)])
        (check-equal? s (apply string-append (map (lambda (t) (token-text c t)) (vector->list (cst-tokens c)))))))))

(test "quoted data and syntax templates are told apart, and each ends under its unquote"
  (let* ([s "(list 'a `(b ,c) #'(d #,e) '#'f `#'g)"] [c (read-text s)]
         [state (lambda (name)
                  (let ([t (find (lambda (t) (and (eq? (token-kind t) 'symbol) (string=? (token-text c t) name)))
                                 (vector->list (cst-tokens c)))])
                    (list (cst-quoted-data? c t) (cst-syntax-template? c t))))])
    (check-equal? '((#f #f) (#t #f) (#t #f) (#f #f) (#f #t) (#f #f) (#t #f) (#t #f))
      (map state '("list" "a" "b" "c" "d" "e" "f" "g")))))

(test "public procedures check their arguments"
  (check-raises assertion-violation? (read-cst "a" chez-mode))
  (check-raises assertion-violation? (read-cst (make-text "a") 'r6rs))
  (check-raises assertion-violation? (make-reader-mode 'r7rs 'fold))
  (check-raises assertion-violation? (cst-token-at (read-text "a") 5))
  (check-raises assertion-violation? (cst-library-header (read-text "a") 'a)))

(test-exit)
