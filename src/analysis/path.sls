(library (analysis path)
  (export
    ;; URIs and canonical paths
    uri->path path->uri canonical-path normalise-path
    ;; relative paths and globs
    path-relative-to compile-glob glob? glob-pattern glob-match?
    ;; name patterns
    wildcard-match?)
  (import (chezscheme) (analysis posix))

  (define (hex-value c)
    (cond
      [(char<=? #\0 c #\9) (fx- (char->integer c) 48)]
      [(char<=? #\a c #\f) (fx- (char->integer c) 87)]
      [(char<=? #\A c #\F) (fx- (char->integer c) 55)]
      [else #f]))

  (define (percent-decode s start end)
    (let loop ([i start] [bytes '()])
      (cond
        [(fx= i end)
         (let ([bv (u8-list->bytevector (reverse bytes))])
           (and (not (memv 0 bytes))
                (let ([decoded (utf8->string bv)])
                  (and (bytevector=? bv (string->utf8 decoded)) decoded))))]
        [(char=? (string-ref s i) #\%)
         (let ([high (and (fx< (fx+ i 2) end) (hex-value (string-ref s (fx+ i 1))))]
               [low (and (fx< (fx+ i 2) end) (hex-value (string-ref s (fx+ i 2))))])
           (and high low (loop (fx+ i 3) (cons (fx+ (fx* 16 high) low) bytes))))]
        [else
         (let ([c (string-ref s i)])
           (and (fx< (char->integer c) 128) (loop (fx+ i 1) (cons (char->integer c) bytes))))])))

  (define (prefix-ci? prefix s)
    (and (fx<= (string-length prefix) (string-length s))
         (string-ci=? prefix (substring s 0 (string-length prefix)))))

  ;; A query or fragment is not part of the path; a file name holding ? or #
  ;; arrives percent-encoded.
  (define (path-end s start)
    (let loop ([i start])
      (if (or (fx= i (string-length s)) (memv (string-ref s i) '(#\? #\#)))
          i
          (loop (fx+ i 1)))))

  (define (uri->path uri)
    (unless (string? uri) (assertion-violationf 'uri->path "~s is not a string" uri))
    (let ([path-start (cond
                        [(prefix-ci? "file:///" uri) 7]
                        [(prefix-ci? "file://localhost/" uri) 16]
                        [(and (prefix-ci? "file:/" uri) (not (prefix-ci? "file://" uri))) 5]
                        [else #f])])
      (and path-start (percent-decode uri path-start (path-end uri path-start)))))

  (define (unreserved? b)
    (or (fx<= 65 b 90) (fx<= 97 b 122) (fx<= 48 b 57) (memv b '(45 46 95 126))))

  (define (path->uri path)
    (unless (and (string? path) (fx> (string-length path) 0) (char=? (string-ref path 0) #\/))
      (assertion-violationf 'path->uri "~s is not an absolute path" path))
    (call-with-string-output-port
      (lambda (out)
        (put-string out "file://")
        (bytevector-for-each
          (lambda (b)
            (if (or (unreserved? b) (fx= b 47))
                (put-char out (integer->char b))
                (fprintf out "%~2,'0X" b)))
          (string->utf8 path)))))

  (define (bytevector-for-each f bv)
    (do ([i 0 (fx+ i 1)]) ((fx= i (bytevector-length bv))) (f (bytevector-u8-ref bv i))))

  (define (split-path p)
    (let loop ([i 0] [start 0] [parts '()])
      (cond
        [(fx= i (string-length p)) (reverse (cons (substring p start i) parts))]
        [(char=? (string-ref p i) #\/) (loop (fx+ i 1) (fx+ i 1) (cons (substring p start i) parts))]
        [else (loop (fx+ i 1) start parts)])))

  (define (normalise-path p)
    (let loop ([parts (split-path p)] [kept '()])
      (cond
        [(null? parts)
         (if (null? kept)
             "/"
             (apply string-append (map (lambda (part) (string-append "/" part)) (reverse kept))))]
        [(member (car parts) '("" ".")) (loop (cdr parts) kept)]
        [(string=? (car parts) "..") (loop (cdr parts) (if (null? kept) kept (cdr kept)))]
        [else (loop (cdr parts) (cons (car parts) kept))])))

  (define (canonical-path p)
    (unless (and (string? p) (fx> (string-length p) 0))
      (assertion-violationf 'canonical-path "~s is not a path" p))
    (let ([absolute (if (char=? (string-ref p 0) #\/) p (string-append (current-directory) "/" p))])
      (or (real-path absolute) (normalise-path absolute))))

  (define (path-components p)
    (filter (lambda (part) (not (string=? part ""))) (split-path p)))

  (define (path-relative-to path base)
    (unless (string? path) (assertion-violationf 'path-relative-to "~s is not a path" path))
    (unless (string? base) (assertion-violationf 'path-relative-to "~s is not a path" base))
    (let loop ([p (path-components (normalise-path path))] [b (path-components (normalise-path base))])
      (cond
        [(null? b) (if (null? p) "." (join-components p))]
        [(and (pair? p) (string=? (car p) (car b))) (loop (cdr p) (cdr b))]
        [else #f])))

  (define (join-components parts)
    (let loop ([parts (cdr parts)] [acc (car parts)])
      (if (null? parts) acc (loop (cdr parts) (string-append acc "/" (car parts))))))

  (define-record-type glob
    (nongenerative) (sealed #t)
    (fields pattern base components))

  (define (compile-glob pattern base)
    (unless (and (string? pattern) (fx> (string-length pattern) 0))
      (assertion-violationf 'compile-glob "~s is not a glob" pattern))
    (unless (and (string? base) (fx> (string-length base) 0) (char=? (string-ref base 0) #\/))
      (assertion-violationf 'compile-glob "~s is not an absolute directory" base))
    (make-glob pattern (normalise-path base) (path-components pattern)))

  ;; Only the last star is a backtrack point: whatever an earlier star
  ;; would take instead, the later one can take as well. So the time is the
  ;; pattern's length times the input's, however many stars there are.
  (define (sequence-match? pattern-length input-length star? same?)
    (let loop ([i 0] [j 0] [star #f] [resume 0])
      (cond
        [(and (fx< i pattern-length) (star? i)) (loop (fx+ i 1) j i j)]
        [(and (fx< i pattern-length) (fx< j input-length) (same? i j)) (loop (fx+ i 1) (fx+ j 1) star resume)]
        [(and (fx= i pattern-length) (fx= j input-length)) #t]
        [(and star (fx< resume input-length)) (loop (fx+ star 1) (fx+ resume 1) star (fx+ resume 1))]
        [else #f])))

  (define (component-match? pattern name)
    (sequence-match? (string-length pattern) (string-length name)
      (lambda (i) (char=? (string-ref pattern i) #\*))
      (lambda (i j) (let ([c (string-ref pattern i)]) (or (char=? c #\?) (char=? c (string-ref name j)))))))

  (define (wildcard-match? pattern name)
    (unless (string? pattern) (assertion-violationf 'wildcard-match? "~s is not a pattern" pattern))
    (unless (string? name) (assertion-violationf 'wildcard-match? "~s is not a string" name))
    (sequence-match? (string-length pattern) (string-length name)
      (lambda (i) (char=? (string-ref pattern i) #\*))
      (lambda (i j) (char=? (string-ref pattern i) (string-ref name j)))))

  (define (components-match? patterns names)
    (let ([patterns (list->vector patterns)] [names (list->vector names)])
      (sequence-match? (vector-length patterns) (vector-length names)
        (lambda (i) (string=? (vector-ref patterns i) "**"))
        (lambda (i j) (component-match? (vector-ref patterns i) (vector-ref names j))))))

  (define (glob-match? g path)
    (unless (glob? g) (assertion-violationf 'glob-match? "~s is not a glob" g))
    (unless (string? path) (assertion-violationf 'glob-match? "~s is not a path" path))
    (let ([relative (path-relative-to path (glob-base g))])
      (and relative
           (not (string=? relative "."))
           (components-match? (glob-components g) (path-components relative))))))
