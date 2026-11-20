(import (chezscheme) (check) (analysis diagnostic))

(test "a diagnostic keeps its fields"
  (let* ([r (make-related "a.ss" 1 2 "here")]
         [d (make-diagnostic 'read-error (severity error) 3 5 "bad" (list r)
              (diagnostic-tag-set unnecessary))])
    (check-equal? 'read-error (diagnostic-rule d))
    (check-equal? 'error (diagnostic-severity d))
    (check-equal? '(3 5) (list (diagnostic-start d) (diagnostic-end d)))
    (check-equal? "bad" (diagnostic-message d))
    (check-equal? (list r) (diagnostic-related d))
    (check-equal? '(unnecessary) (enum-set->list (diagnostic-tags d)))
    (check-equal? '("a.ss" 1 2 "here")
      (list (related-path r) (related-start r) (related-end r) (related-message r)))))

(test "a diagnostic refuses an empty message, a reversed span and an unknown severity"
  (check-raises assertion-violation?
    (make-diagnostic 'read-error (severity error) 0 1 "" '() (diagnostic-tag-set)))
  (check-raises assertion-violation?
    (make-diagnostic 'read-error (severity error) 2 1 "m" '() (diagnostic-tag-set)))
  (check-raises assertion-violation?
    (make-diagnostic 'read-error 'fatal 0 1 "m" '() (diagnostic-tag-set)))
  (check-raises assertion-violation?
    (make-diagnostic "read-error" (severity error) 0 1 "m" '() (diagnostic-tag-set))))

(test-exit)
