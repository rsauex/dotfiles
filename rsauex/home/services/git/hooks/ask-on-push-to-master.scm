(define-module (rsauex home services git hooks ask-on-push-to-master)
  #:use-module ((ice-9 regex))
  #:use-module ((rsauex script utils))
  #:use-module ((srfi srfi-1))
  #:use-module ((srfi srfi-69))

  #:export (check))

(define (ask-confirmation ref)
  (with-input-from-tty
   (lambda ()
     (ask (string-append "Are you sure you want to push to \"" ref "\"? (y/n): ")))))

(define (check . _)
  (let ((remote-refs (make-hash-table string=? string-hash)))
    (for-each (lambda (line)
                (let ((parts (string-split line #\space)))
                  (hash-table-set! remote-refs (third parts) #t)))
              (get-lines (current-input-port)))
    (for-each (lambda (ref)
                (when (string-match "^refs/heads/(master|main|trunk|dev)" ref)
                  (let ((answer (ask-confirmation ref)))
                    (unless (or (string= "y" answer) (string= "Y" answer))
                      (exit 1)))))
              (hash-table-keys remote-refs)))
  (exit 0))
