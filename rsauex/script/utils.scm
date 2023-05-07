(define-module (rsauex script utils)
  #:use-module ((ice-9 popen))
  #:use-module ((ice-9 ports))
  #:use-module ((ice-9 textual-ports))

  #:export (invoke/capture-string-all
            invoke/capture-strings
            get-lines
            ask
            with-input-from-tty))

(define (invoke/capture-string-all program . args)
  (let* ((pipe (apply open-pipe* OPEN_READ program args))
         (output (get-string-all pipe)))
    (close-pipe pipe)
    output))

(define (invoke/capture-strings program . args)
  (let* ((pipe (apply open-pipe* OPEN_READ program args))
         (lines (get-lines pipe)))
    (close-pipe pipe)
    lines))

(define (get-lines port)
  (let ((lines (list))
        (line (get-line port)))
    (while (not (eof-object? line))
      (set! lines (cons line lines))
      (set! line (get-line port)))
    (reverse! lines)))

(define (ask question)
  (display question)
  (force-output)
  (get-line (current-input-port)))

(define (with-input-from-tty thunk)
  (call-with-input-file "/dev/tty"
    (lambda (port)
      (with-input-from-port port thunk))))
