(define-module (rsauex script template eval)
  #:use-module ((guix gexp))
  #:use-module ((rsauex home config)))

(define-public args
  (make-parameter '()))

(define (get-arg key)
  (assq-ref (args) key))
