(import (chezscheme) (check) (analysis posix) (analysis threads))

(define (read-all fd)
  (let ([port (open-fd-input-port fd)])
    (let ([bv (get-bytevector-all port)])
      (close-port port)
      (if (eof-object? bv) "" (utf8->string bv)))))

(define (run-shell command fd-map directory)
  (let-values ([(ours theirs) (make-socket-pair)])
    (let ([pid (spawn-process "/bin/sh" (list "sh" "-c" command) (cons (cons 1 theirs) fd-map) directory)])
      (close-fd theirs)
      (let* ([output (read-all ours)] [status (process-wait-exit pid)])
        (process-reap! pid)
        (values output status)))))

(test "a spawned child gets the mapped descriptors, crossed ones included"
  (let-values ([(a1 a2) (make-socket-pair)] [(b1 b2) (make-socket-pair)])
    (let ([low (fxmin a2 b2)] [high (fxmax a2 b2)])
      (let-values ([(output status)
                    (run-shell (format "printf low >&~a; printf high >&~a" high low)
                      (list (cons low high) (cons high low)) "/")])
        (close-fd a2)
        (close-fd b2)
        (check-equal? 0 (exit-status-code status))
        (let ([from-a (read-all a1)] [from-b (read-all b1)])
          (if (fx< a2 b2)
              (check-equal? '("low" "high") (list from-a from-b))
              (check-equal? '("high" "low") (list from-a from-b))))))))

(test "a spawned child runs in its directory, leads its process group and holds no other descriptor"
  (let-values ([(output status)
                (run-shell "pwd; test $(cut -d' ' -f5 /proc/$$/stat) = $$ && echo leader; ls /proc/$$/fd | sort -n | tr '\\n' ' '"
                  '() "/tmp")])
    (check-equal? 0 (exit-status-code status))
    (check-equal? "/tmp\nleader\n0 1 2 " output)))

(test "exit codes and signals are decoded"
  (let-values ([(output status) (run-shell "exit 3" '() "/")])
    (check-equal? '(3 #f) (list (exit-status-code status) (exit-status-signal status))))
  (let-values ([(output status) (run-shell "kill -TERM $$" '() "/")])
    (check-equal? (list #f signal-terminate) (list (exit-status-code status) (exit-status-signal status)))))

(test "a process group is signalled before its leader is reaped"
  (let ([pid (spawn-process "/bin/sh" '("sh" "-c" "sleep 30 & wait") '() "/")])
    (sleep (make-time 'time-duration 100000000 0))
    (process-group-signal! pid signal-kill)
    (let ([status (process-wait-exit pid)])
      (check-equal? signal-kill (exit-status-signal status))
      (process-reap! pid)
      (check-equal? #f (process-wait-exit pid)))))

(test "a child of a process that ignores SIGPIPE, as Chez does, has no ignored or blocked signal"
  (let-values ([(output status)
                (run-shell "grep -E '^Sig(Ign|Blk)' /proc/$$/status" '() "/")])
    (check-equal? "SigBlk:\t0000000000000000\nSigIgn:\t0000000000000000\n" output)))

(test "duplicated descriptors are close-on-exec and land above 2"
  (let-values ([(a b) (make-socket-pair)])
    (let ([d (duplicate-fd a)])
      (check (fx> d 2))
      (let-values ([(output status) (run-shell (format "test -e /proc/$$/fd/~a && echo open || echo closed" d) '() "/")])
        (check-equal? "closed\n" output))
      (duplicate-fd-onto b d)
      (write-fd d (string->utf8 "through d"))
      (close-fd d)
      (close-fd b)
      (check-equal? "through d" (read-all a)))))

(test "a process watch ends with its process"
  (let ([pid (spawn-process "/bin/sh" '("sh" "-c" "sleep 0.2") '() "/")])
    (let ([w (open-process-watch pid)])
      (check (not (process-watch-ended? w)))
      (process-wait-exit pid)
      (check (process-watch-ended? w))
      (process-reap! pid)
      (check (process-watch-ended? w))
      (check-equal? #f (open-process-watch pid)))))

(test "real-path resolves links and reports absence as #f"
  (check-equal? "/" (real-path "/tmp/.."))
  (check-equal? #f (real-path "/nonexistent/x"))
  (check-equal? #f (real-path "/etc/passwd/x"))
  (check-raises posix-error? (real-path (make-string 5000 #\a)))
  (check-equal? "/" (real-path "/proc/self/../..")))

(test "the executable path is the running Chez"
  (check (string=? "scheme" (path-last (self-executable-path)))))

(test "bad arguments are assertion violations"
  (check-raises assertion-violation? (spawn-process "/bin/true" '() '() "/"))
  (check-raises assertion-violation? (spawn-process "/bin/true" '("true") '((1 . -1)) "/"))
  (check-raises assertion-violation? (process-signal! 0 signal-kill))
  (check-raises posix-error? (spawn-process "/nonexistent" '("x") '() "/")))


(define temporary-root
  (let ([dir (format "/tmp/cz-posix-test-~a" (process-id))])
    (system (format "rm -rf ~a; mkdir -p ~a" dir dir))
    dir))

(define (temporary-file name) (string-append temporary-root "/" name))

(define (write-text! file text)
  (call-with-output-file file (lambda (out) (put-string out text)) 'truncate))

;; A probe is a script run by a fresh Chez that loads this repository's
;; libraries, so a limit it sets cannot reach the test process.
(define (run-probe forms)
  (let ([file (temporary-file "probe.ss")])
    (call-with-output-file file
      (lambda (out)
        (write '(source-directories '("src")) out)
        (write '(library-directories '(("src" . "build/obj/src"))) out)
        (for-each (lambda (form) (write form out) (newline out)) forms))
      'truncate)
    (run-shell (format "exec ~a -q --script ~a 2>/dev/null" (self-executable-path) file) '() (current-directory))))

(define (seconds-since start)
  (let ([d (time-difference (current-time 'time-monotonic) start)])
    (+ (time-second d) (/ (time-nanosecond d) 1e9))))

(test "a child given descriptors 0 to 4 holds exactly those"
  (let-values ([(a b) (make-socket-pair)])
    (let ([null (open-null-fd)])
      (let-values ([(output status)
                    (run-shell "ls /proc/$$/fd | sort -n | tr '\\n' ' '; printf three >&3; printf four >&4"
                      (list (cons 0 null) (cons 2 b) (cons 3 b) (cons 4 b)) "/")])
        (close-fd null)
        (close-fd b)
        (check-equal? "0 1 2 3 4 " output)
        (check-equal? "threefour" (read-all a))))))

(test "a blocked process-wait-exit does not delay collections"
  (let* ([pid (spawn-process "/bin/sh" '("sh" "-c" "sleep 1") '() "/")] [done (make-mailbox)])
    (fork-thread (lambda () (mailbox-post! done (process-wait-exit pid))))
    (sleep (make-time 'time-duration 100000000 0))
    (let ([start (current-time 'time-monotonic)])
      (do ([i 0 (fx+ i 1)]) ((fx= i 20)) (collect))
      (check (< (seconds-since start) 0.5)))
    (check-equal? 0 (exit-status-code (mailbox-take! done)))
    (process-reap! pid)))

(test "shutdown-fd wakes a reader whose peer a grandchild holds, after the queued bytes"
  (let-values ([(ours theirs) (make-socket-pair)])
    (let ([pid (spawn-process "/bin/sh" '("sh" "-c" "printf queued >&3; sleep 30 & exit 0")
                 (list (cons 3 theirs)) "/")]
          [read (make-mailbox)])
      (close-fd theirs)
      (process-wait-exit pid)
      (fork-thread (lambda () (mailbox-post! read (read-all ours))))
      (sleep (make-time 'time-duration 200000000 0))
      (shutdown-fd ours)
      (let ([r (mailbox-take! read (deadline-after 2))])
        (check-equal? "queued" r))
      (process-group-signal! pid signal-kill)
      (process-reap! pid))))

(test "shutdown-fd wakes a writer blocked on a full socket whose peer a grandchild holds"
  (let-values ([(ours theirs) (make-socket-pair)])
    (let ([pid (spawn-process "/bin/sh" '("sh" "-c" "sleep 30 & exit 0") (list (cons 3 theirs)) "/")]
          [written (make-mailbox)])
      (close-fd theirs)
      (process-wait-exit pid)
      (fork-thread
        (lambda ()
          (mailbox-post! written
            (guard (c [(posix-error? c) 'failed])
              (write-fd ours (make-bytevector (* 16 1024 1024) 0))
              'written))))
      (sleep (make-time 'time-duration 200000000 0))
      (check (mailbox-timeout? (mailbox-take! written (deadline-after 0))))
      (shutdown-fd ours)
      (check-equal? 'failed (mailbox-take! written (deadline-after 2)))
      (close-fd ours)
      (process-group-signal! pid signal-kill)
      (process-reap! pid))))

(define c-signal-ignore (foreign-procedure "signal" (int uptr) uptr))
(define SIG_IGN 1)

(test "a child spawned while SIGCHLD and SIGHUP are ignored has default dispositions, and the kernel reaps it"
  (c-signal-ignore signal-child SIG_IGN)
  (c-signal-ignore signal-hangup SIG_IGN)
  (let-values ([(ours theirs) (make-socket-pair)])
    (let ([pid (spawn-process "/bin/sh" (list "sh" "-c" "grep -E '^Sig(Ign|Blk)' /proc/$$/status")
                 (list (cons 1 theirs)) "/")])
      (close-fd theirs)
      (check-equal? "SigBlk:\t0000000000000000\nSigIgn:\t0000000000000000\n" (read-all ours))
      (check-equal? #f (process-wait-exit pid))))
  (set-signal-default! signal-child)
  (set-signal-default! signal-hangup))

(test "the memory budget is the smallest of physical memory and the cgroup limits"
  (let ([budget (memory-budget-bytes)])
    (check (> budget 0))
    (let ([limit (let ([cg (call-with-input-file "/proc/self/cgroup" get-line)])
                   (let ([file (string-append "/sys/fs/cgroup" (substring cg 3 (string-length cg)) "/memory.max")])
                     (and (file-exists? file)
                          (string->number (call-with-input-file file get-line)))))])
      (when limit (check (<= budget limit))))
    (let ([cg (let ([line (call-with-input-file "/proc/self/cgroup" get-line)])
                (string-append "/sys/fs/cgroup" (substring line 3 (string-length line))))]
          [child (format "cz-test-~a" (process-id))])
      (if (guard (c [#t #f]) (mkdir (string-append cg "/" child)) #t)
          (let ([file (temporary-file "probe.ss")])
            (call-with-output-file file
              (lambda (out)
                (for-each (lambda (form) (write form out) (newline out))
                  `((source-directories '("src"))
                    (library-directories '(("src" . "build/obj/src")))
                    (import (analysis posix))
                    (write (memory-budget-bytes)))))
              'truncate)
            (let-values ([(output status)
                          (run-shell (format "echo $$ > ~a/~a/cgroup.procs && exec ~a -q --script ~a"
                                       cg child (self-executable-path) file)
                            '() (current-directory))])
              (check-equal? 0 (exit-status-code status))
              (check-equal? budget (string->number output)))
            (guard (c [#t #f]) (delete-directory (string-append cg "/" child))))
          (printf "memory budget in a child cgroup: skipped, cannot create a cgroup under ~a\n" cg)))))

(test "an address-space limit ends a child that allocates with SIGABRT"
  (let-values ([(output status)
                (run-probe '((import (analysis posix))
                             (set-address-space-limit! (* 300 1024 1024))
                             (disable-core-dumps!)
                             (let loop ([keep '()]) (loop (cons (make-bytevector (* 1024 1024)) keep)))))])
    (check-equal? 6 (exit-status-signal status))))

(test "a child can raise its own OOM score and end itself at once"
  (let-values ([(output status)
                (run-probe '((import (analysis posix))
                             (set-oom-score-adjust! 1000)
                             (display (call-with-input-file "/proc/self/oom_score_adj" get-line))
                             (flush-output-port (current-output-port))
                             (dynamic-wind void (lambda () (exit-immediately 7)) (lambda () (display "unwound")))))])
    (check-equal? 7 (exit-status-code status))
    (check-equal? "1000" output)))

(test "real-path reports a permission problem rather than absence"
  (let ([dir (temporary-file "locked")])
    (mkdir dir)
    (write-text! (string-append dir "/f") "x")
    (chmod dir #o000)
    (if (file-exists? (string-append dir "/f"))
        (printf "real-path permission check: skipped, the test runs with access to every directory\n")
        (check-raises posix-error? (real-path (string-append dir "/f"))))
    (chmod dir #o700)))

(test "a file stamp changes on an atomic replace with the same size and modification time"
  (let ([a (temporary-file "a")] [b (temporary-file "b")])
    (write-text! a "first")
    (let ([before (file-stamp a)])
      (write-text! b "other")
      (system (format "touch -r ~a ~a && mv ~a ~a" a b b a))
      (let ([after (file-stamp a)])
        (check-equal? (list-ref (file-stamp->list before) 2) (list-ref (file-stamp->list after) 2))
        (check-equal? (list-ref (file-stamp->list before) 3) (list-ref (file-stamp->list after) 3))
        (check (not (file-stamp=? before after)))
        (check (file-stamp=? after (list->file-stamp (file-stamp->list after))))))
    (check-equal? #f (file-stamp (temporary-file "missing")))
    (check-equal? #f (file-stamp (string-append a "/below-a-file")))))

(test "make-temporary-directory makes a new directory only its owner can enter"
  (let* ([prefix (temporary-file "made-")]
         [a (make-temporary-directory prefix)]
         [b (make-temporary-directory prefix)])
    (check (not (string=? a b)))
    (for-each
      (lambda (d)
        (let ([st (file-status d)])
          (check (string=? prefix (substring d 0 (string-length prefix))))
          (check-equal? '(directory #o700) (list (file-status-type st) (file-status-mode st)))))
      (list a b))
    (check-raises posix-error? (make-temporary-directory (temporary-file "missing/made-")))))

(test "file-status counts hard links, and file-writable? answers for the effective user"
  (let ([a (temporary-file "linked")] [b (temporary-file "linked-again")])
    (write-text! a "x")
    (check-equal? 1 (file-status-links (file-status a)))
    (check (file-writable? a))
    (system (format "ln ~a ~a && chmod 444 ~a" a b a))
    (check-equal? 2 (file-status-links (file-status a)))
    (check-equal? (= (effective-user-id) 0) (file-writable? a))
    (check-raises posix-error? (file-writable? (string-append a "/below-a-file")))))

(system (format "rm -rf ~a" temporary-root))

(test "a directory's lock is held by one open at a time, is free once that is closed, and delete-tree! removes it"
  (let* ([dir (make-temporary-directory (format "/tmp/cz-lock-~a-" (process-id)))]
         [held (lock-directory dir #t)])
    (check (fixnum? held))
    (check-equal? #f (lock-directory dir #f))
    (close-fd held)
    (let ([again (lock-directory dir #f)])
      (check (fixnum? again))
      (close-fd again))
    (mkdir (string-append dir "/inner") #o700)
    (call-with-output-file (string-append dir "/inner/file") (lambda (out) (put-string out "x")))
    (delete-tree! dir)
    (check (not (file-exists? dir)))
    (check-equal? #f (lock-directory dir #f))))

(test-exit)
