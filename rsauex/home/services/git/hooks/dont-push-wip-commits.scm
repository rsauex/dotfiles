(define-module (rsauex home services git hooks dont-push-wip-commits)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-69)
  #:use-module (rsauex script utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)

  #:export (check))

(define zero
  (make-string 40 #\0))

(define wip-commit-regex
  "^\\(WIP\\|FIXUP\\|AMEND\\|SQUASH\\)")

(define (find-wip-commits-in-range range)
  (invoke/capture-strings "git" "rev-list"
                          "--regexp-ignore-case"
                          "--grep" wip-commit-regex range))

(define (check . _)
  (let ((wip-commits (list)))

    (define (add-wip-commits commits)
      (set! wip-commits (lset-union string=? wip-commits commits)))

    (for-each (lambda (line)
                (match (string-split line #\space)
                  ((local-ref local-oid _ remote-oid)
                   (unless (string=? local-oid zero)
                     (let ((range (if (string=? remote-oid zero)
                                      local-oid
                                      (string-append remote-oid ".." local-oid))))
                       (add-wip-commits (find-wip-commits-in-range range)))))))
              (get-lines (current-input-port)))

    (unless (null? wip-commits)
      (format #t "WIP commits found:~%~{  ~a~%~}" wip-commits)
      (exit 1)))

  (exit 0))
