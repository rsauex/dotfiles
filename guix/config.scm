(define (get-os-module host)
  (resolve-module (list 'rsauex 'system 'pc (string->symbol host)) #:ensure #f))

(let* ((host (gethostname))
       (os-module (get-os-module host)))
  (unless os-module
    (raise-exception "No configuration for current host"))
  (module-ref os-module '%os))
