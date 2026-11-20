(library (analysis posix)
  (export
    ;; sockets and descriptors
    make-socket-pair shutdown-fd close-fd open-null-fd set-close-on-exec!
    duplicate-fd duplicate-fd-onto write-fd
    ;; processes
    process-id? spawn-process process-signal! process-group-signal! process-wait-exit process-poll-exit
    process-reap!
    exit-status? exit-status-code exit-status-signal
    open-process-watch process-watch-ended?
    self-executable-path set-signal-default! exit-immediately process-id
    ;; signals
    signal-hangup signal-interrupt signal-kill signal-pipe signal-terminate signal-child
    ;; limits and memory
    set-address-space-limit! disable-core-dumps! set-oom-score-adjust! memory-budget-bytes
    ;; files
    real-path file-stamp file-stamp? file-stamp=? file-stamp->list list->file-stamp
    file-status file-status? file-status-type file-status-owner file-status-mode file-status-links
    effective-user-id file-writable?
    sync-path! rename-path! make-temporary-directory lock-directory delete-tree!
    create-temporary-file set-fd-mode! sync-fd!
    path-stamp path-stamp=? unreadable-stamp? unreadable-stamp-reason
    ;; errors
    &posix-error make-posix-error posix-error? posix-error-errno)
  (import (chezscheme))
  (include "analysis/include/posix.ss"))
