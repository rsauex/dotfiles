(define-module (rsauex script template eval)
  #:use-module ((guix gexp)))

(define-public args
  (make-parameter '()))

(define (get-arg key)
  (assq-ref (args) key))
