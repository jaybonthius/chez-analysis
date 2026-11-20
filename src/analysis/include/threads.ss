(define-record-type mailbox
  (nongenerative) (sealed #t)
  (fields mutex condition capacity (mutable head) (mutable tail) (mutable count) keyed
          (mutable closed?) (mutable dropped))
  (protocol
    (lambda (new)
      (define (make capacity)
        (new (make-mutex) (make-condition) capacity '() '() 0 (make-hashtable equal-hash equal?) #f 0))
      (case-lambda
        [() (make #f)]
        [(capacity)
         (unless (and (fixnum? capacity) (fx> capacity 0))
           (assertion-violationf 'make-mailbox "~s is not a positive fixnum" capacity))
         (make capacity)]))))

;; Made here and never posted, so no message can be mistaken for it.
(define mailbox-timeout-object (string->uninterned-symbol "mailbox-timeout"))
(define (mailbox-timeout? x) (eq? x mailbox-timeout-object))

(define no-key (string->uninterned-symbol "no-key"))

(define (check-mailbox who mb)
  (unless (mailbox? mb) (assertion-violationf who "~s is not a mailbox" mb)))

(define (check-deadline who d)
  (unless (and (time? d) (eq? (time-type d) 'time-monotonic))
    (assertion-violationf who "~s is not a deadline" d)))

(define mailbox-post!
  (case-lambda
    [(mb x) (mailbox-post! mb x no-key)]
    [(mb x key)
     (check-mailbox 'mailbox-post! mb)
     (with-mutex (mailbox-mutex mb)
       (let ([queued (and (not (eq? key no-key)) (hashtable-ref (mailbox-keyed mb) key #f))])
         (cond
           [(mailbox-closed? mb) (void)]
           [queued (set-cdr! queued x)]
           [(and (mailbox-capacity mb) (fx>= (mailbox-count mb) (mailbox-capacity mb)))
            (mailbox-dropped-set! mb (fx+ (mailbox-dropped mb) 1))]
           [else
            (let ([entry (cons key x)])
              (let ([cell (list entry)])
                (if (null? (mailbox-head mb))
                    (mailbox-head-set! mb cell)
                    (set-cdr! (mailbox-tail mb) cell))
                (mailbox-tail-set! mb cell))
              (unless (eq? key no-key) (hashtable-set! (mailbox-keyed mb) key entry))
              (mailbox-count-set! mb (fx+ (mailbox-count mb) 1))
              (condition-signal (mailbox-condition mb)))])))]))

(define (pop! mb)
  (let ([entry (car (mailbox-head mb))])
    (mailbox-head-set! mb (cdr (mailbox-head mb)))
    (mailbox-count-set! mb (fx- (mailbox-count mb) 1))
    (unless (eq? (car entry) no-key) (hashtable-delete! (mailbox-keyed mb) (car entry)))
    (cdr entry)))

;; The remaining time is recomputed from the monotonic clock before each
;; wait, so a step of the wall clock cannot move a deadline.
(define (wait-for-item! mb deadline)
  (let loop ()
    (cond
      [(fx> (mailbox-count mb) 0) #t]
      [(mailbox-closed? mb) #f]
      [(not deadline) (condition-wait (mailbox-condition mb) (mailbox-mutex mb)) (loop)]
      [else
       (let ([now (current-time 'time-monotonic)])
         (if (time<=? deadline now)
             #f
             (begin
               (condition-wait (mailbox-condition mb) (mailbox-mutex mb) (time-difference deadline now))
               (loop))))])))

(define mailbox-take!
  (case-lambda
    [(mb) (take! 'mailbox-take! mb #f)]
    [(mb deadline)
     (check-deadline 'mailbox-take! deadline)
     (take! 'mailbox-take! mb deadline)]))

(define (take! who mb deadline)
  (check-mailbox who mb)
  (with-mutex (mailbox-mutex mb)
    (if (wait-for-item! mb deadline) (pop! mb) mailbox-timeout-object)))

(define (mailbox-take-all! mb)
  (check-mailbox 'mailbox-take-all! mb)
  (with-mutex (mailbox-mutex mb)
    (if (wait-for-item! mb #f)
        (let ([items (map cdr (mailbox-head mb))])
          (mailbox-head-set! mb '())
          (mailbox-tail-set! mb '())
          (mailbox-count-set! mb 0)
          (hashtable-clear! (mailbox-keyed mb))
          items)
        '())))

(define (mailbox-close! mb)
  (check-mailbox 'mailbox-close! mb)
  (with-mutex (mailbox-mutex mb)
    (mailbox-closed?-set! mb #t)
    (condition-broadcast (mailbox-condition mb))))

(define (mailbox-dropped-count mb)
  (check-mailbox 'mailbox-dropped-count mb)
  (with-mutex (mailbox-mutex mb) (mailbox-dropped mb)))

(define (deadline-after seconds)
  (unless (and (real? seconds) (>= seconds 0))
    (assertion-violationf 'deadline-after "~s is not a number of seconds" seconds))
  (let ([ns (exact (round (* seconds 1000000000)))])
    (add-duration (current-time 'time-monotonic)
      (make-time 'time-duration (remainder ns 1000000000) (quotient ns 1000000000)))))

(define (deadline-passed? d)
  (check-deadline 'deadline-passed? d)
  (time<=? d (current-time 'time-monotonic)))

(define-record-type once-cell
  (nongenerative) (sealed #t)
  (fields mutex (mutable value) (mutable state))
  (protocol
    (lambda (new)
      (lambda () (new (make-mutex) #f 'unset)))))

;; The thunk runs under the cell's mutex, so other threads wait for its
;; value. Chez's mutexes are recursive: a force from inside the thunk would
;; reenter instead of waiting, and is refused.
(define (once-cell-force cell thunk)
  (unless (once-cell? cell) (assertion-violationf 'once-cell-force "~s is not a once-cell" cell))
  (with-mutex (once-cell-mutex cell)
    (case (once-cell-state cell)
      [(set) (once-cell-value cell)]
      [(computing) (assertion-violationf 'once-cell-force "~s is forced from its own thunk" cell)]
      [else
       (once-cell-state-set! cell 'computing)
       (let ([finished? #f])
         (dynamic-wind
           void
           (lambda ()
             (let ([v (thunk)])
               (once-cell-value-set! cell v)
               (set! finished? #t)
               v))
           (lambda () (once-cell-state-set! cell (if finished? 'set 'unset)))))])))

(define thread-port-maker
  (make-parameter #f
    (lambda (maker)
      (unless (or (not maker) (procedure? maker))
        (assertion-violationf 'thread-port-maker "~s is not a procedure" maker))
      maker)))

;; An unhandled error ends a fork-thread thread silently, and a port shared
;; with another thread can be corrupted, so each thread gets its own port
;; and reports its failure. Closing the port on every exit posts its last
;; partial line, which is often the failure's message.
(define (fork-guarded-thread name thunk failed)
  (let ([make-port (thread-port-maker)])
    (unless make-port
      (assertion-violationf 'fork-guarded-thread "thread-port-maker is not set"))
    (fork-thread
      (lambda ()
        (let ([out (make-port name)])
          (dynamic-wind
            void
            (lambda ()
              (parameterize ([current-output-port out] [current-error-port out])
                (guard (c [#t (failed name c)]) (thunk))))
            (lambda () (close-port out))))))))

;; Output size 0 makes Chez pass every write straight to the write
;; procedure; with a buffer of Chez's own, output would reach the procedure
;; only when the buffer filled. The line buffer is locked because threads
;; forked by a thread that writes to the port inherit it as their
;; current-output-port.
(define make-line-output-port
  (case-lambda
    [(name post-line!) (make-line-output-port name post-line! (lambda (post!) (void)))]
    [(name post-line! receive-pending-poster)
     (let ([lock (make-mutex)] [pending (open-output-string)])
       (define (post-pending!)
         (with-mutex lock
           (let ([s (get-output-string pending)])
             (unless (string=? s "") (post-line! s)))))
       (let ([port (make-custom-textual-output-port (symbol->string name)
                     (lambda (s start count)
                       (with-mutex lock
                         (do ([i start (fx+ i 1)]) ((fx= i (fx+ start count)))
                           (let ([c (string-ref s i)])
                             (if (char=? c #\newline)
                                 (post-line! (get-output-string pending))
                                 (write-char c pending)))))
                       count)
                     #f #f post-pending!)])
         (set-textual-port-output-size! port 0)
         (receive-pending-poster post-pending!)
         port))]))
