(library (analysis diagnostic)
  (export
    severity severity-set
    make-diagnostic diagnostic? diagnostic-rule diagnostic-severity
    diagnostic-start diagnostic-end diagnostic-message diagnostic-related diagnostic-tags
    make-related related? related-path related-start related-end related-message
    diagnostic-tag diagnostic-tag-set)
  (import (chezscheme))

  (define-enumeration severity (error warning information hint) severity-set)
  (define-enumeration diagnostic-tag (unnecessary deprecated) diagnostic-tag-set)

  (define (check-span who start end)
    (unless (and (fixnum? start) (fixnum? end) (fx<= 0 start end))
      (assertion-violationf who "~s and ~s are not a span" start end)))

  ;; An empty message makes VS Code fail an assertion, so it is refused here.
  (define (check-message who message)
    (unless (and (string? message) (not (fx= (string-length message) 0)))
      (assertion-violationf who "~s is not a non-empty string" message)))

  (define-record-type related
    (nongenerative) (sealed #t)
    (fields path start end message)
    (protocol
      (lambda (new)
        (lambda (path start end message)
          (unless (string? path) (assertion-violationf 'make-related "~s is not a path" path))
          (check-span 'make-related start end)
          (check-message 'make-related message)
          (new path start end message)))))

  (define-record-type diagnostic
    (nongenerative) (sealed #t)
    (fields rule severity start end message related tags)
    (protocol
      (lambda (new)
        (lambda (rule severity start end message related tags)
          (unless (symbol? rule) (assertion-violationf 'make-diagnostic "~s is not a rule name" rule))
          (unless (enum-set-member? severity (enum-set-universe (severity-set)))
            (assertion-violationf 'make-diagnostic "~s is not a severity" severity))
          (check-span 'make-diagnostic start end)
          (check-message 'make-diagnostic message)
          (unless (and (list? related) (andmap related? related))
            (assertion-violationf 'make-diagnostic "~s is not a list of related information" related))
          (unless (and (enum-set? tags)
                       (enum-set-subset? tags (enum-set-universe (diagnostic-tag-set))))
            (assertion-violationf 'make-diagnostic "~s is not a diagnostic tag set" tags))
          (new rule severity start end message related tags))))))
