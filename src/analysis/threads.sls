(library (analysis threads)
  (export
    ;; mailboxes
    make-mailbox mailbox? mailbox-post! mailbox-take! mailbox-take-all! mailbox-close!
    mailbox-timeout? mailbox-dropped-count
    ;; deadlines
    deadline-after deadline-passed?
    ;; once-cells
    make-once-cell once-cell? once-cell-force
    ;; threads and their ports
    fork-guarded-thread thread-port-maker make-line-output-port)
  (import (chezscheme))
  (include "analysis/include/threads.ss"))
