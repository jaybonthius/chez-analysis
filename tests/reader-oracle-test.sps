(import (chezscheme) (check) (analysis text) (analysis reader) (reader-oracle))

(define (chez-condition s)
  (guard (e [#t e]) (read (open-string-input-port s)) #f))

(define field-type-error "'#[#{profile-counter b5vnnom9h4o4uny0-2} 0e]")

(test "a record whose constructor rejects a field is skipped as process-dependent"
  (let ([comparison (compare-with-chez field-type-error 'chezscheme 'no-fold)])
    (check-equal? '() (comparison-failures comparison))
    (check (comparison-skipped comparison))))

(test "a condition without a span is accepted only from constructing a record of the text"
  (let ([c (read-cst (make-text field-type-error) (make-reader-mode 'chezscheme 'no-fold))]
        [condition (chez-condition field-type-error)])
    (check (raised-by-record-construction? c condition))
    (check (not (raised-by-record-construction? c (make-message-condition "other"))))
    (check (not (raised-by-record-construction?
                  (read-cst (make-text "'(#{profile-counter b5vnnom9h4o4uny0-2} 0e)") (make-reader-mode 'chezscheme 'no-fold))
                  condition)))))

(test-exit)
