(import (chezscheme) (check) (analysis threads))

(define (elapsed-seconds thunk)
  (let ([start (current-time 'time-monotonic)])
    (thunk)
    (let ([d (time-difference (current-time 'time-monotonic) start)])
      (+ (time-second d) (/ (time-nanosecond d) 1e9)))))

(define (join-all threads) (for-each thread-join threads))

(test "a once-cell computes once under racing threads"
  (let ([cell (make-once-cell)] [calls 0] [m (make-mutex)] [results (make-mailbox)])
    (join-all
      (map (lambda (i)
             (fork-thread
               (lambda ()
                 (mailbox-post! results
                   (once-cell-force cell
                     (lambda ()
                       (with-mutex m (set! calls (+ calls 1)))
                       (sleep (make-time 'time-duration 20000000 0))
                       (list 'value i)))))))
           (iota 8)))
    (check-equal? 1 calls)
    (let ([forced (map (lambda (i) (mailbox-take! results)) (iota 8))])
      (check (for-all (lambda (v) (eq? v (car forced))) forced)))))

(test "a once-cell whose thunk raises stays unset"
  (let ([cell (make-once-cell)])
    (check-raises error? (once-cell-force cell (lambda () (error 'thunk "failed"))))
    (check-equal? 7 (once-cell-force cell (lambda () 7)))
    (check-equal? 7 (once-cell-force cell (lambda () 8)))))

(test "forcing a once-cell from its own thunk raises"
  (let ([cell (make-once-cell)])
    (check-raises assertion-violation?
      (once-cell-force cell (lambda () (once-cell-force cell (lambda () 1)))))
    (check-equal? 2 (once-cell-force cell (lambda () 2)))))

(test "each producer's items arrive in order"
  (let ([mb (make-mailbox)] [n 5000])
    (let ([producers (map (lambda (p)
                            (fork-thread
                              (lambda ()
                                (do ([i 0 (fx+ i 1)]) ((fx= i n))
                                  (mailbox-post! mb (cons p i))))))
                          (iota 4))])
      (let loop ([k 0] [next (make-vector 4 0)])
        (if (fx= k (fx* 4 n))
            (check-equal? (make-vector 4 n) next)
            (let ([item (mailbox-take! mb)])
              (check-equal? (vector-ref next (car item)) (cdr item))
              (vector-set! next (car item) (fx+ (cdr item) 1))
              (loop (fx+ k 1) next))))
      (join-all producers))))

(test "a keyed post replaces the queued item in its place"
  (let ([mb (make-mailbox)])
    (mailbox-post! mb 'a "k")
    (mailbox-post! mb 'b)
    (mailbox-post! mb 'c "k")
    (mailbox-post! mb 'd (string #\j))
    (check-equal? '(c b d) (mailbox-take-all! mb))
    (mailbox-post! mb 'e "k")
    (check-equal? 'e (mailbox-take! mb))))

(test "a bounded mailbox drops and counts"
  (let ([mb (make-mailbox 2)])
    (for-each (lambda (x) (mailbox-post! mb x)) '(1 2 3 4))
    (check-equal? 2 (mailbox-dropped-count mb))
    (check-equal? '(1 2) (mailbox-take-all! mb))
    (check-raises assertion-violation? (make-mailbox 0))))

(test "a closed mailbox drops posts and never blocks"
  (let ([mb (make-mailbox)])
    (mailbox-post! mb 1)
    (mailbox-close! mb)
    (mailbox-post! mb 2)
    (check-equal? 1 (mailbox-take! mb))
    (check (mailbox-timeout? (mailbox-take! mb)))
    (check-equal? '() (mailbox-take-all! mb))))

(test "closing wakes a waiting consumer"
  (let ([mb (make-mailbox)] [out (make-mailbox)])
    (fork-thread (lambda () (mailbox-post! out (mailbox-take-all! mb))))
    (sleep (make-time 'time-duration 50000000 0))
    (mailbox-close! mb)
    (check-equal? '() (mailbox-take! out (deadline-after 5)))))

(test "a deadline returns the timeout object on time"
  (let ([mb (make-mailbox)])
    (let ([t (elapsed-seconds (lambda () (check (mailbox-timeout? (mailbox-take! mb (deadline-after 0.1))))))])
      (check (<= 0.1 t 0.2)))
    (check (mailbox-timeout? (mailbox-take! mb (deadline-after 0))))
    (check-equal? 'time-monotonic (time-type (deadline-after 1)))
    (check (deadline-passed? (deadline-after 0)))
    (check (not (deadline-passed? (deadline-after 10))))
    (check-raises assertion-violation? (mailbox-take! mb (current-time 'time-utc)))))

(test "an item posted before the deadline is taken"
  (let ([mb (make-mailbox)])
    (fork-thread (lambda () (sleep (make-time 'time-duration 30000000 0)) (mailbox-post! mb 'late)))
    (check-equal? 'late (mailbox-take! mb (deadline-after 5)))))

(define lines (make-mailbox))

(define (posting-maker name)
  (make-line-output-port name (lambda (line) (mailbox-post! lines (cons name line)))))

(define (take-lines n)
  (let loop ([k 0] [taken '()])
    (if (fx= k n)
        (reverse taken)
        (loop (fx+ k 1) (cons (mailbox-take! lines (deadline-after 5)) taken)))))

(test "a guarded thread's lines, partial line and failure all arrive"
  (parameterize ([thread-port-maker posting-maker])
    (fork-guarded-thread 'worker
      (lambda ()
        (display "hello line\n")
        (display "partial")
        (error 'worker "broke"))
      (lambda (name c)
        (display " ")
        (display (condition-message c))
        (newline))))
  (check-equal? '((worker . "hello line") (worker . "partial broke")) (take-lines 2)))

(test "a guarded thread that returns posts its last partial line"
  (parameterize ([thread-port-maker posting-maker])
    (fork-guarded-thread 'quiet (lambda () (write-char #\a) (write-char #\b)) (lambda (name c) #f)))
  (check-equal? '((quiet . "ab")) (take-lines 1)))

(test "lines written by several threads to one port arrive whole"
  (let* ([count 20000]
         [received (make-mailbox)]
         [port (make-line-output-port 'shared (lambda (line) (mailbox-post! received line)))]
         [line-of (lambda (k i) (format "thread ~a writes line ~a of a long enough text" k i))])
    (join-all
      (map (lambda (k)
             (fork-thread
               (lambda ()
                 (do ([i 0 (fx+ i 1)]) ((fx= i count))
                   (display (string-append (line-of k i) "\n") port)))))
           (iota 5)))
    (let ([all (mailbox-take-all! received)])
      (check-equal? (* 5 count) (length all))
      (let ([expected (make-hashtable string-hash string=?)])
        (do ([k 0 (fx+ k 1)]) ((fx= k 5))
          (do ([i 0 (fx+ i 1)]) ((fx= i count))
            (hashtable-set! expected (line-of k i) #t)))
        (check (for-all (lambda (line) (hashtable-ref expected line #f)) all))))))

(test "fork-guarded-thread needs a port maker"
  (parameterize ([thread-port-maker #f])
    (check-raises assertion-violation? (fork-guarded-thread 'x void void))))

(test-exit)
