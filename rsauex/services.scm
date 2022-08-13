(define-module (rsauex services)
  #:use-module (ice-9 match)
  #:use-module (gnu services)

  #:export (anon-service))

(define (make-anon-service name target/value-list)
  (let* ((extensions (map (match-lambda
                            ((target . value)
                             (service-extension target (const value))))
                          target/value-list))
         (type (service-type (name name)
                             (extensions extensions)
                             (default-value #f)
                             (description "This is a simple service."))))
    (service type)))

(define-syntax anon-service
  (syntax-rules ()
    ((_ name (target value) ...)
     (make-anon-service (quote name) (list  (cons target value) ...)))))
