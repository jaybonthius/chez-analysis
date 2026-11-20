(define libc
  (guard (c [#t (errorf 'cz "this program runs only on Linux with the GNU C library (libc.so.6)")])
    (load-shared-object "libc.so.6")))

(define-condition-type &posix-error &error make-posix-error posix-error?
  (errno posix-error-errno))

(define strerror (foreign-procedure "strerror" (int) string))

(define (posix-error who call errno)
  (raise (condition (make-posix-error errno) (make-who-condition who)
           (make-message-condition (format "~a failed: ~a" call (strerror errno))))))

(define EINTR 4)
(define ENOTCONN 107)
(define ENOENT 2)
(define ESRCH 3)
(define ECHILD 10)
(define ENOTDIR 20)
(define EEXIST 17)
(define ENOTEMPTY 39)
(define EWOULDBLOCK 11)
(define EACCES 13)
(define EPERM 1)
(define EROFS 30)
(define ETXTBSY 26)

(define signal-hangup 1)
(define signal-interrupt 2)
(define signal-kill 9)
(define signal-pipe 13)
(define signal-terminate 15)
(define signal-child 17)

(define (check-fd who fd)
  (unless (and (fixnum? fd) (fx>= fd 0)) (assertion-violationf who "~s is not a file descriptor" fd)))

(define (process-id? x) (and (fixnum? x) (fx< 0 x #x80000000)))

(define (check-pid who pid)
  (unless (process-id? pid) (assertion-violationf who "~s is not a process id" pid)))

(define (check-signal who sig)
  (unless (and (fixnum? sig) (fx< 0 sig 65)) (assertion-violationf who "~s is not a signal number" sig)))

(define (check-string who s)
  (unless (string? s) (assertion-violationf who "~s is not a string" s)))

(define (retrying-on-interrupt call)
  (let-values ([(r errno) (call)])
    (if (and (eqv? r -1) (eqv? errno EINTR))
        (retrying-on-interrupt call)
        (values r errno))))

(define c-fcntl (foreign-procedure __errno (__varargs_after 2) "fcntl" (int int int) int))
(define c-dup2 (foreign-procedure __errno "dup2" (int int) int))
(define c-close (foreign-procedure __errno "close" (int) int))
(define c-write (foreign-procedure __collect_safe __errno "write" (int uptr size_t) ssize_t))
(define c-socketpair (foreign-procedure __errno "socketpair" (int int int u8*) int))
(define c-signal (foreign-procedure __errno "signal" (int uptr) iptr))
(define c-kill (foreign-procedure __errno "kill" (int int) int))
;; glibc wraps pidfd_open only from 2.36; the system call needs Linux 5.3,
;; and has this number on every architecture Chez runs on.
(define SYS_pidfd_open 434)
(define c-syscall-2 (foreign-procedure __errno (__varargs_after 1) "syscall" (long int unsigned) long))
(define c-poll (foreign-procedure __errno "poll" (u8* unsigned-long int) int))
(define c-realpath (foreign-procedure __collect_safe __errno "realpath" (uptr uptr) uptr))
(define c-readlink (foreign-procedure __errno "readlink" (string u8* size_t) ssize_t))
(define c-waitid (foreign-procedure __collect_safe __errno "waitid" (int int uptr int) int))
(define c-waitpid (foreign-procedure __errno "waitpid" (int uptr int) int))
(define c-shutdown (foreign-procedure __errno "shutdown" (int int) int))
(define c-open (foreign-procedure __errno "open" (string int) int))
(define c-exit (foreign-procedure "_exit" (int) void))
(define c-getpid (foreign-procedure "getpid" () int))
(define c-setrlimit (foreign-procedure __errno "setrlimit" (int u8*) int))
(define c-sysconf (foreign-procedure "sysconf" (int) long))
(define c-stat (foreign-procedure __collect_safe __errno "stat" (uptr uptr) int))
(define c-lstat (foreign-procedure __collect_safe __errno "lstat" (uptr uptr) int))
(define c-geteuid (foreign-procedure "geteuid" () unsigned))
(define c-faccessat (foreign-procedure __errno "faccessat" (int string int int) int))
(define c-fsync (foreign-procedure __collect_safe __errno "fsync" (int) int))
(define c-rename (foreign-procedure __collect_safe __errno "rename" (uptr uptr) int))
(define c-mkdtemp (foreign-procedure __collect_safe __errno "mkdtemp" (uptr) uptr))
(define c-flock (foreign-procedure __collect_safe __errno "flock" (int int) int))
(define c-mkostemp (foreign-procedure __collect_safe __errno "mkostemp" (uptr int) int))
(define c-fchmod (foreign-procedure __errno "fchmod" (int unsigned) int))

(define F_SETFD 2)
(define F_DUPFD_CLOEXEC 1030)
(define FD_CLOEXEC 1)

(define (duplicate-fd fd)
  (check-fd 'duplicate-fd fd)
  (let-values ([(r errno) (c-fcntl fd F_DUPFD_CLOEXEC 3)])
    (when (fx< r 0) (posix-error 'duplicate-fd "fcntl" errno))
    r))

(define (duplicate-fd-onto from to)
  (check-fd 'duplicate-fd-onto from)
  (check-fd 'duplicate-fd-onto to)
  (let-values ([(r errno) (retrying-on-interrupt (lambda () (c-dup2 from to)))])
    (when (fx< r 0) (posix-error 'duplicate-fd-onto "dup2" errno))))

(define (set-close-on-exec! fd)
  (check-fd 'set-close-on-exec! fd)
  (let-values ([(r errno) (c-fcntl fd F_SETFD FD_CLOEXEC)])
    (when (fx< r 0) (posix-error 'set-close-on-exec! "fcntl" errno))))

;; close is not retried after EINTR: Linux has released the descriptor by
;; then, and a retry could close one another thread has just opened.
(define (close-fd fd)
  (check-fd 'close-fd fd)
  (let-values ([(r errno) (c-close fd)])
    (when (and (fx< r 0) (not (eqv? errno EINTR))) (posix-error 'close-fd "close" errno))))

;; The bytes are copied out of the Scheme heap, so the call can let the
;; collector run while write blocks.
(define (write-fd fd bv)
  (check-fd 'write-fd fd)
  (unless (bytevector? bv) (assertion-violationf 'write-fd "~s is not a bytevector" bv))
  (let* ([n (bytevector-length bv)] [buffer (foreign-alloc (fxmax n 1))])
    (dynamic-wind
      void
      (lambda ()
        (do ([i 0 (fx+ i 1)]) ((fx= i n)) (foreign-set! 'unsigned-8 buffer i (bytevector-u8-ref bv i)))
        (let loop ([written 0])
          (unless (fx= written n)
            (let-values ([(r errno) (c-write fd (+ buffer written) (fx- n written))])
              (cond
                [(fx>= r 0) (loop (fx+ written r))]
                [(eqv? errno EINTR) (loop written)]
                [else (posix-error 'write-fd "write" errno)])))))
      (lambda () (foreign-free buffer)))))

(define AF_UNIX 1)
(define SOCK_STREAM 1)
(define SOCK_CLOEXEC #x80000)

(define (make-socket-pair)
  (let ([fds (make-bytevector 8)])
    (let-values ([(r errno) (c-socketpair AF_UNIX (fxior SOCK_STREAM SOCK_CLOEXEC) 0 fds)])
      (when (fx< r 0) (posix-error 'make-socket-pair "socketpair" errno))
      (values (bytevector-s32-native-ref fds 0) (bytevector-s32-native-ref fds 4)))))

(define (set-signal-default! sig)
  (check-signal 'set-signal-default! sig)
  (let-values ([(r errno) (c-signal sig 0)])
    (when (fx= r -1) (posix-error 'set-signal-default! "signal" errno))))

(define (kill! who pid sig)
  (let-values ([(r errno) (c-kill pid sig)])
    (when (fx< r 0) (posix-error who "kill" errno))))

(define (process-signal! pid sig)
  (check-pid 'process-signal! pid)
  (check-signal 'process-signal! sig)
  (kill! 'process-signal! pid sig))

(define (process-group-signal! pid sig)
  (check-pid 'process-group-signal! pid)
  (check-signal 'process-group-signal! sig)
  (kill! 'process-group-signal! (fx- pid) sig))

(define-record-type process-watch
  (nongenerative) (sealed #t)
  (fields fd))

;; A pidfd stays bound to its process, so a reused pid cannot make a dead
;; process look alive.
(define (open-process-watch pid)
  (check-pid 'open-process-watch pid)
  (let-values ([(fd errno) (c-syscall-2 SYS_pidfd_open pid 0)])
    (cond
      [(fx>= fd 0) (make-process-watch fd)]
      [(eqv? errno ESRCH) #f]
      [else (posix-error 'open-process-watch "pidfd_open" errno)])))

(define POLLIN 1)

(define (process-watch-ended? w)
  (unless (process-watch? w) (assertion-violationf 'process-watch-ended? "~s is not a process watch" w))
  (let ([pollfd (make-bytevector 8 0)])
    (bytevector-s32-native-set! pollfd 0 (process-watch-fd w))
    (bytevector-s16-native-set! pollfd 4 POLLIN)
    (let-values ([(r errno) (retrying-on-interrupt (lambda () (c-poll pollfd 1 0)))])
      (when (fx< r 0) (posix-error 'process-watch-ended? "poll" errno))
      (fx> r 0))))

(define PATH_MAX 4096)

(define (foreign-c-string p)
  (let loop ([n 0])
    (if (fx= (foreign-ref 'unsigned-8 p n) 0)
        (let ([bv (make-bytevector n)])
          (do ([i 0 (fx+ i 1)]) ((fx= i n)) (bytevector-u8-set! bv i (foreign-ref 'unsigned-8 p i)))
          (utf8->string bv))
        (loop (fx+ n 1)))))

;; realpath reads the file system, which can block on a network mount, so
;; its arguments live outside the Scheme heap and the collector can run.
(define (real-path p)
  (check-string 'real-path p)
  (let ([path (c-string-copy p)] [buffer (foreign-alloc (fx+ PATH_MAX 1))])
    (dynamic-wind
      void
      (lambda ()
        (let-values ([(r errno) (c-realpath path buffer)])
          (cond
            [(not (eqv? r 0)) (foreign-c-string buffer)]
            [(or (eqv? errno ENOENT) (eqv? errno ENOTDIR)) #f]
            [else (posix-error 'real-path "realpath" errno)])))
      (lambda () (foreign-free path) (foreign-free buffer)))))

(define (self-executable-path)
  (let ([buffer (make-bytevector PATH_MAX)])
    (let-values ([(n errno) (c-readlink "/proc/self/exe" buffer PATH_MAX)])
      (when (fx< n 0) (posix-error 'self-executable-path "readlink" errno))
      (let ([out (make-bytevector n)])
        (bytevector-copy! buffer 0 out 0 n)
        (utf8->string out)))))

(define-record-type exit-status
  (nongenerative) (sealed #t)
  (fields code signal))

(define P_PID 1)
(define WEXITED 4)
(define WNOWAIT #x01000000)
(define WNOHANG 1)
(define CLD_EXITED 1)

;; WNOWAIT leaves the child unreaped, so its pid cannot be reused before the
;; caller has finished signalling its process group.
(define (process-wait-exit pid)
  (check-pid 'process-wait-exit pid)
  (let ([info (foreign-alloc 128)])
    (dynamic-wind
      void
      (lambda ()
        (let loop ()
          (let-values ([(r errno) (c-waitid P_PID pid info (fxior WEXITED WNOWAIT))])
            (cond
              [(fx= r 0)
               (let ([code (foreign-ref 'int info 8)] [status (foreign-ref 'int info 24)])
                 (if (fx= code CLD_EXITED)
                     (make-exit-status status #f)
                     (make-exit-status #f status)))]
              [(eqv? errno EINTR) (loop)]
              [(eqv? errno ECHILD) #f]
              [else (posix-error 'process-wait-exit "waitid" errno)]))))
      (lambda () (foreign-free info)))))

(define (process-poll-exit pid)
  (check-pid 'process-poll-exit pid)
  (let ([info (foreign-alloc 128)])
    (dynamic-wind
      void
      (lambda ()
        (do ([i 0 (fx+ i 8)]) ((fx= i 128)) (foreign-set! 'unsigned-64 info i 0))
        (let loop ()
          (let-values ([(r errno) (c-waitid P_PID pid info (fxior WEXITED WNOWAIT WNOHANG))])
            (cond
              [(fx= r 0)
               (and (not (fx= (foreign-ref 'int info 16) 0))
                    (let ([code (foreign-ref 'int info 8)] [status (foreign-ref 'int info 24)])
                      (if (fx= code CLD_EXITED)
                          (make-exit-status status #f)
                          (make-exit-status #f status))))]
              [(eqv? errno EINTR) (loop)]
              [(eqv? errno ECHILD) (make-exit-status #f #f)]
              [else (posix-error 'process-poll-exit "waitid" errno)]))))
      (lambda () (foreign-free info)))))

(define (process-reap! pid)
  (check-pid 'process-reap! pid)
  (let-values ([(r errno) (retrying-on-interrupt (lambda () (c-waitpid pid 0 WNOHANG)))])
    (when (and (fx< r 0) (not (eqv? errno ECHILD))) (posix-error 'process-reap! "waitpid" errno))))

(define c-spawn (foreign-procedure "posix_spawn" (uptr string uptr uptr uptr uptr) int))
(define c-actions-init (foreign-procedure "posix_spawn_file_actions_init" (uptr) int))
(define c-actions-destroy (foreign-procedure "posix_spawn_file_actions_destroy" (uptr) int))
(define c-actions-dup2 (foreign-procedure "posix_spawn_file_actions_adddup2" (uptr int int) int))
(define c-actions-chdir (foreign-procedure "posix_spawn_file_actions_addchdir_np" (uptr string) int))
(define c-actions-closefrom (foreign-procedure "posix_spawn_file_actions_addclosefrom_np" (uptr int) int))
(define c-attr-init (foreign-procedure "posix_spawnattr_init" (uptr) int))
(define c-attr-destroy (foreign-procedure "posix_spawnattr_destroy" (uptr) int))
(define c-attr-flags (foreign-procedure "posix_spawnattr_setflags" (uptr short) int))
(define c-attr-pgroup (foreign-procedure "posix_spawnattr_setpgroup" (uptr int) int))
(define c-attr-sigmask (foreign-procedure "posix_spawnattr_setsigmask" (uptr uptr) int))
(define c-attr-sigdefault (foreign-procedure "posix_spawnattr_setsigdefault" (uptr uptr) int))
(define c-sigemptyset (foreign-procedure "sigemptyset" (uptr) int))
(define c-sigfillset (foreign-procedure "sigfillset" (uptr) int))

(define POSIX_SPAWN_SETPGROUP 2)
(define POSIX_SPAWN_SETSIGDEF 4)
(define POSIX_SPAWN_SETSIGMASK 8)

;; Larger than glibc's posix_spawn_file_actions_t, posix_spawnattr_t and
;; sigset_t, so a change of their size cannot overrun the allocation.
(define spawn-object-size 1024)

(define (check-spawn-result call r)
  (unless (fx= r 0) (posix-error 'spawn-process call r)))

(define (c-string-copy s)
  (let* ([bv (string->utf8 s)] [n (bytevector-length bv)] [p (foreign-alloc (fx+ n 1))])
    (do ([i 0 (fx+ i 1)]) ((fx= i n)) (foreign-set! 'unsigned-8 p i (bytevector-u8-ref bv i)))
    (foreign-set! 'unsigned-8 p n 0)
    p))

;; File actions run in order in the child. Every parent descriptor is first
;; copied above every number the map names, then onto its target: applied in
;; map order, a crossed map such as ((5 . 6) (6 . 5)) gives the child the same
;; descriptor twice. Closing everything above the targets keeps out files that
;; another thread opened without close-on-exec, the temporaries included.
(define (spawn-process exe argv fd-map directory)
  (check-string 'spawn-process exe)
  (unless (and (list? argv) (pair? argv) (andmap string? argv))
    (assertion-violationf 'spawn-process "~s is not a non-empty list of strings" argv))
  (unless (and (list? fd-map)
               (andmap (lambda (m) (and (pair? m) (fixnum? (car m)) (fx>= (car m) 0)
                                        (fixnum? (cdr m)) (fx>= (cdr m) 0)))
                       fd-map))
    (assertion-violationf 'spawn-process "~s is not a list of (child-fd . parent-fd)" fd-map))
  (check-string 'spawn-process directory)
  (let* ([actions (foreign-alloc spawn-object-size)]
         [attributes (foreign-alloc spawn-object-size)]
         [mask (foreign-alloc spawn-object-size)]
         [defaults (foreign-alloc spawn-object-size)]
         [pid-cell (foreign-alloc 8)]
         [arguments (map c-string-copy argv)]
         [argument-vector (foreign-alloc (fx* 8 (fx+ (length argv) 1)))]
         [initialised '()])
    (dynamic-wind
      void
      (lambda ()
        (check-spawn-result "posix_spawn_file_actions_init" (c-actions-init actions))
        (set! initialised (cons (lambda () (c-actions-destroy actions)) initialised))
        (check-spawn-result "posix_spawnattr_init" (c-attr-init attributes))
        (set! initialised (cons (lambda () (c-attr-destroy attributes)) initialised))
        (let* ([named (append (map car fd-map) (map cdr fd-map))]
               [temporary-base (fx+ 1 (apply fxmax 2 named))])
          (let loop ([m fd-map] [i 0])
            (unless (null? m)
              (check-spawn-result "posix_spawn_file_actions_adddup2"
                (c-actions-dup2 actions (cdar m) (fx+ temporary-base i)))
              (loop (cdr m) (fx+ i 1))))
          (let loop ([m fd-map] [i 0])
            (unless (null? m)
              (check-spawn-result "posix_spawn_file_actions_adddup2"
                (c-actions-dup2 actions (fx+ temporary-base i) (caar m)))
              (loop (cdr m) (fx+ i 1))))
          (check-spawn-result "posix_spawn_file_actions_addclosefrom_np"
            (c-actions-closefrom actions (fx+ 1 (apply fxmax 2 (map car fd-map))))))
        (check-spawn-result "posix_spawn_file_actions_addchdir_np" (c-actions-chdir actions directory))
        (check-spawn-result "posix_spawnattr_setflags"
          (c-attr-flags attributes (fxior POSIX_SPAWN_SETPGROUP POSIX_SPAWN_SETSIGMASK POSIX_SPAWN_SETSIGDEF)))
        (check-spawn-result "posix_spawnattr_setpgroup" (c-attr-pgroup attributes 0))
        (c-sigemptyset mask)
        (check-spawn-result "posix_spawnattr_setsigmask" (c-attr-sigmask attributes mask))
        (c-sigfillset defaults)
        (check-spawn-result "posix_spawnattr_setsigdefault" (c-attr-sigdefault attributes defaults))
        (let loop ([a arguments] [i 0])
          (if (null? a)
              (foreign-set! 'uptr argument-vector (fx* 8 i) 0)
              (begin
                (foreign-set! 'uptr argument-vector (fx* 8 i) (car a))
                (loop (cdr a) (fx+ i 1)))))
        (check-spawn-result "posix_spawn"
          (c-spawn pid-cell exe actions attributes argument-vector
            (foreign-ref 'uptr (foreign-entry "environ") 0)))
        (foreign-ref 'int pid-cell 0))
      (lambda ()
        (for-each (lambda (destroy) (destroy)) initialised)
        (for-each foreign-free (append arguments (list actions attributes mask defaults pid-cell argument-vector)))))))

(define SHUT_RDWR 2)

;; A reader or writer blocked on the other end of the socket wakes with end
;; of file or EPIPE even while another process still holds its peer, which
;; closing the descriptor would not do.
(define (shutdown-fd fd)
  (check-fd 'shutdown-fd fd)
  (let-values ([(r errno) (c-shutdown fd SHUT_RDWR)])
    (when (and (fx< r 0) (not (eqv? errno ENOTCONN))) (posix-error 'shutdown-fd "shutdown" errno))))

(define O_RDWR 2)
(define O_CLOEXEC #o2000000)

(define (open-null-fd)
  (let-values ([(fd errno) (retrying-on-interrupt (lambda () (c-open "/dev/null" (fxior O_RDWR O_CLOEXEC))))])
    (when (fx< fd 0) (posix-error 'open-null-fd "open" errno))
    fd))

;; _exit runs no handler, so project code in the process cannot intercept it.
(define (exit-immediately status)
  (unless (and (fixnum? status) (fx<= 0 status 255))
    (assertion-violationf 'exit-immediately "~s is not an exit status" status))
  (c-exit status))

(define (process-id) (c-getpid))

(define RLIMIT_CORE 4)
(define RLIMIT_AS 9)

(define (set-resource-limit! who resource bytes)
  (let ([limit (make-bytevector 16)])
    (bytevector-u64-native-set! limit 0 bytes)
    (bytevector-u64-native-set! limit 8 bytes)
    (let-values ([(r errno) (c-setrlimit resource limit)])
      (when (fx< r 0) (posix-error who "setrlimit" errno)))))

(define (set-address-space-limit! bytes)
  (unless (and (integer? bytes) (exact? bytes) (> bytes 0))
    (assertion-violationf 'set-address-space-limit! "~s is not a positive number of bytes" bytes))
  (set-resource-limit! 'set-address-space-limit! RLIMIT_AS bytes))

(define (disable-core-dumps!)
  (set-resource-limit! 'disable-core-dumps! RLIMIT_CORE 0))

(define (set-oom-score-adjust! n)
  (unless (and (fixnum? n) (fx<= -1000 n 1000))
    (assertion-violationf 'set-oom-score-adjust! "~s is not an OOM score adjustment" n))
  (call-with-output-file "/proc/self/oom_score_adj"
    (lambda (out) (put-string out (number->string n)))
    'truncate))

(define _SC_PAGESIZE 30)
(define _SC_PHYS_PAGES 85)

(define (read-file-line file)
  (and (file-exists? file)
       (guard (c [(i/o-error? c) #f])
         (call-with-input-file file get-line))))

(define (cgroup-v2-path)
  (let ([text (guard (c [(i/o-error? c) #f])
                (call-with-input-file "/proc/self/cgroup" get-string-all))])
    (and (string? text)
         (let loop ([lines (split-string text #\newline)])
           (cond
             [(null? lines) #f]
             [(and (fx>= (string-length (car lines)) 3) (string=? (substring (car lines) 0 3) "0::"))
              (substring (car lines) 3 (string-length (car lines)))]
             [else (loop (cdr lines))])))))

(define (split-string s c)
  (let loop ([i 0] [start 0] [parts '()])
    (cond
      [(fx= i (string-length s)) (reverse (cons (substring s start i) parts))]
      [(char=? (string-ref s i) c) (loop (fx+ i 1) (fx+ i 1) (cons (substring s start i) parts))]
      [else (loop (fx+ i 1) start parts)])))

;; A limit is often set on a slice or on the user's service while the
;; process's own cgroup says max, so every ancestor is read.
(define (cgroup-memory-limit)
  (let ([path (cgroup-v2-path)])
    (and path
         (let loop ([parts (filter (lambda (p) (not (string=? p ""))) (split-string path #\/))] [limit #f])
           (let* ([dir (apply string-append "/sys/fs/cgroup" (map (lambda (p) (string-append "/" p)) parts))]
                  [value (read-file-line (string-append dir "/memory.max"))]
                  [n (and (string? value) (string->number value 10))]
                  [limit (if (and n (or (not limit) (< n limit))) n limit)])
             (if (null? parts) limit (loop (reverse (cdr (reverse parts))) limit)))))))

(define (memory-budget-bytes)
  (let ([physical (* (c-sysconf _SC_PHYS_PAGES) (c-sysconf _SC_PAGESIZE))]
        [limit (cgroup-memory-limit)])
    (if limit (min limit physical) physical)))

(define-record-type (stamp make-file-stamp file-stamp?)
  (nongenerative) (sealed #t)
  (fields device inode size modified-seconds modified-nanoseconds changed-seconds changed-nanoseconds))

(define (file-stamp=? a b)
  (unless (file-stamp? a) (assertion-violationf 'file-stamp=? "~s is not a file stamp" a))
  (unless (file-stamp? b) (assertion-violationf 'file-stamp=? "~s is not a file stamp" b))
  (equal? (file-stamp->list a) (file-stamp->list b)))

(define (file-stamp->list s)
  (unless (file-stamp? s) (assertion-violationf 'file-stamp->list "~s is not a file stamp" s))
  (list (stamp-device s) (stamp-inode s) (stamp-size s)
        (stamp-modified-seconds s) (stamp-modified-nanoseconds s)
        (stamp-changed-seconds s) (stamp-changed-nanoseconds s)))

(define (list->file-stamp l)
  (unless (and (list? l) (fx= (length l) 7) (andmap (lambda (x) (and (integer? x) (exact? x))) l))
    (assertion-violationf 'list->file-stamp "~s is not a list of seven exact integers" l))
  (apply make-file-stamp l))

(define stat-size 256)

;; The inode and the change time catch an editor's atomic replace, and a
;; same-size edit within one tick of a coarse modification time. stat can
;; block on a network file system, so the collector may run during it.
(define (stat-stamp who p)
  (check-string who p)
  (let ([path (c-string-copy p)] [buffer (foreign-alloc stat-size)])
    (dynamic-wind
      void
      (lambda ()
        (let-values ([(r errno) (c-stat path buffer)])
          (cond
            [(fx= r 0)
             (values
               (make-file-stamp
                 (foreign-ref 'unsigned-64 buffer 0) (foreign-ref 'unsigned-64 buffer 8)
                 (foreign-ref 'integer-64 buffer 48)
                 (foreign-ref 'integer-64 buffer 88) (foreign-ref 'integer-64 buffer 96)
                 (foreign-ref 'integer-64 buffer 104) (foreign-ref 'integer-64 buffer 112))
               #f)]
            [(or (eqv? errno ENOENT) (eqv? errno ENOTDIR)) (values #f #f)]
            [else (values #f errno)])))
      (lambda () (foreign-free path) (foreign-free buffer)))))

(define (file-stamp p)
  (let-values ([(stamp errno) (stat-stamp 'file-stamp p)])
    (if errno (posix-error 'file-stamp "stat" errno) stamp)))

;; A path that cannot be read has its error as its stamp, so it compares
;; equal for as long as the same error persists.
(define-record-type unreadable-stamp
  (nongenerative) (sealed #t)
  (fields errno))

(define (unreadable-stamp-reason s)
  (unless (unreadable-stamp? s) (assertion-violationf 'unreadable-stamp-reason "~s is not an unreadable stamp" s))
  (string-downcase (strerror (unreadable-stamp-errno s))))

(define (path-stamp p)
  (let-values ([(stamp errno) (stat-stamp 'path-stamp p)])
    (if errno (make-unreadable-stamp errno) stamp)))

(define (path-stamp=? a b)
  (define (check x)
    (unless (or (not x) (file-stamp? x) (unreadable-stamp? x))
      (assertion-violationf 'path-stamp=? "~s is not a path stamp" x)))
  (check a)
  (check b)
  (cond
    [(and (file-stamp? a) (file-stamp? b)) (file-stamp=? a b)]
    [(and (unreadable-stamp? a) (unreadable-stamp? b)) (fx= (unreadable-stamp-errno a) (unreadable-stamp-errno b))]
    [else (and (not a) (not b))]))
(define-record-type (file-status-record make-file-status file-status?)
  (nongenerative) (sealed #t)
  (fields (immutable type file-status-type) (immutable owner file-status-owner)
          (immutable mode file-status-mode) (immutable links file-status-links)))

(define S_IFMT #o170000)

(define (file-type-of mode)
  (case (fxlogand mode S_IFMT)
    [(#o040000) 'directory]
    [(#o100000) 'regular]
    [(#o120000) 'symbolic-link]
    [else 'other]))

;; The status of the path itself, not of a file a symbolic link names, so a
;; caller can refuse a link.
(define (file-status p)
  (check-string 'file-status p)
  (let ([path (c-string-copy p)] [buffer (foreign-alloc stat-size)])
    (dynamic-wind
      void
      (lambda ()
        (let-values ([(r errno) (c-lstat path buffer)])
          (cond
            [(fx= r 0)
             (let ([mode (foreign-ref 'unsigned-32 buffer 24)])
               (make-file-status (file-type-of mode) (foreign-ref 'unsigned-32 buffer 28)
                 (fxlogand mode #o7777) (foreign-ref 'unsigned-64 buffer 16)))]
            [(or (eqv? errno ENOENT) (eqv? errno ENOTDIR)) #f]
            [else (posix-error 'file-status "lstat" errno)])))
      (lambda () (foreign-free path) (foreign-free buffer)))))

(define (effective-user-id) (c-geteuid))

(define AT_FDCWD -100)
(define AT_EACCESS #x200)
(define W_OK 2)

;; Asked with the effective user, as opening the file for writing would be.
(define (file-writable? p)
  (check-string 'file-writable? p)
  (let-values ([(r errno) (c-faccessat AT_FDCWD p W_OK AT_EACCESS)])
    (cond
      [(fx= r 0) #t]
      [(memv errno (list EACCES EPERM EROFS ETXTBSY)) #f]
      [else (posix-error 'file-writable? "faccessat" errno)])))

(define O_RDONLY 0)

(define (sync-path! p)
  (check-string 'sync-path! p)
  (let-values ([(fd errno) (retrying-on-interrupt (lambda () (c-open p (fxior O_RDONLY O_CLOEXEC))))])
    (when (fx< fd 0) (posix-error 'sync-path! "open" errno))
    (dynamic-wind
      void
      (lambda ()
        (let-values ([(r errno) (retrying-on-interrupt (lambda () (c-fsync fd)))])
          (when (fx< r 0) (posix-error 'sync-path! "fsync" errno))))
      (lambda () (c-close fd)))))

;; A rename onto a directory that is not empty fails instead of replacing
;; it, which is how one of two concurrent writers learns the other won.
(define (rename-path! from to)
  (check-string 'rename-path! from)
  (check-string 'rename-path! to)
  (let ([a (c-string-copy from)] [b (c-string-copy to)])
    (dynamic-wind
      void
      (lambda ()
        (let-values ([(r errno) (c-rename a b)])
          (cond
            [(fx= r 0) #t]
            [(or (eqv? errno EEXIST) (eqv? errno ENOTEMPTY)) #f]
            [else (posix-error 'rename-path! "rename" errno)])))
      (lambda () (foreign-free a) (foreign-free b)))))

(define O_DIRECTORY #o200000)
(define O_NOFOLLOW #o400000)
(define LOCK_EX 2)
(define LOCK_NB 4)

;; The lock belongs to the open directory, so the kernel releases it when
;; the process that holds it ends, however it ends. #f when another holds
;; it, or the directory is gone.
(define (lock-directory p wait?)
  (check-string 'lock-directory p)
  (let-values ([(fd errno) (retrying-on-interrupt
                             (lambda () (c-open p (fxior O_DIRECTORY O_NOFOLLOW O_CLOEXEC))))])
    (cond
      [(fx>= fd 0)
       (let-values ([(r errno) (retrying-on-interrupt
                                 (lambda () (c-flock fd (if wait? LOCK_EX (fxior LOCK_EX LOCK_NB)))))])
         (cond
           [(fx= r 0) fd]
           [else
            (close-fd fd)
            (if (eqv? errno EWOULDBLOCK) #f (posix-error 'lock-directory "flock" errno))]))]
      [(or (eqv? errno ENOENT) (eqv? errno ENOTDIR)) #f]
      [else (posix-error 'lock-directory "open" errno)])))

(define (delete-tree! p)
  (let ([st (file-status p)])
    (when st
      (if (eq? (file-status-type st) 'directory)
          (begin
            (for-each (lambda (name) (delete-tree! (string-append p "/" name))) (directory-list p))
            (delete-directory p))
          (delete-file p)))))

;; mkdtemp chooses a name no one can predict and creates it with mode 0700,
;; so a name another user or an earlier process left cannot stop it.
(define (make-temporary-directory prefix)
  (check-string 'make-temporary-directory prefix)
  (let ([template (c-string-copy (string-append prefix "XXXXXX"))])
    (dynamic-wind
      void
      (lambda ()
        (let-values ([(r errno) (c-mkdtemp template)])
          (when (eqv? r 0) (posix-error 'make-temporary-directory "mkdtemp" errno))
          (foreign-c-string template)))
      (lambda () (foreign-free template)))))

;; mkostemp creates the file with O_EXCL under a name no one can predict,
;; so neither a file already there nor a second run can be overwritten.
(define (create-temporary-file prefix)
  (check-string 'create-temporary-file prefix)
  (let ([template (c-string-copy (string-append prefix "XXXXXX"))])
    (dynamic-wind
      void
      (lambda ()
        (let-values ([(fd errno) (retrying-on-interrupt (lambda () (c-mkostemp template O_CLOEXEC)))])
          (when (fx< fd 0) (posix-error 'create-temporary-file "mkostemp" errno))
          (values (foreign-c-string template) fd)))
      (lambda () (foreign-free template)))))

(define (set-fd-mode! fd mode)
  (check-fd 'set-fd-mode! fd)
  (unless (and (fixnum? mode) (fx<= 0 mode #o7777)) (assertion-violationf 'set-fd-mode! "~s is not a file mode" mode))
  (let-values ([(r errno) (retrying-on-interrupt (lambda () (c-fchmod fd mode)))])
    (when (fx< r 0) (posix-error 'set-fd-mode! "fchmod" errno))))

(define (sync-fd! fd)
  (check-fd 'sync-fd! fd)
  (let-values ([(r errno) (retrying-on-interrupt (lambda () (c-fsync fd)))])
    (when (fx< r 0) (posix-error 'sync-fd! "fsync" errno))))
