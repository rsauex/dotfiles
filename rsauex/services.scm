(define-module (rsauex services)
  #:use-module ((gnu services))
  #:use-module ((ice-9 match))
  #:use-module ((srfi srfi-26))

  #:export (anon-service))

(define (make-anon-service name target/value-alist)
  ;; Store target/value-alist in the value for (@ (nonguix utils)
  ;; with-transformation) to be able to rewrite it.
  (let* ((extensions (map (match-lambda
                            ((target . _value)
                             (service-extension target (cut assq-ref <> target))))
                          target/value-alist)))
    (service (service-type (name name)
                           (extensions extensions)
                           (description (symbol->string name)))
             target/value-alist)))

(define-syntax anon-service
  (syntax-rules ()
    ((_ name (target value) ...)
     (make-anon-service (quote name) (list (cons target value) ...)))))
