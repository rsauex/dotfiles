(define (get-os-module host)
  (resolve-module (list 'rsauex 'system 'pc host) #:ensure #f))

(display "Machine name: ")
(force-output)
(let* ((host (read))
       (os-module (get-os-module host)))
  (unless os-module
    (raise-exception "No configuration for current host"))
  (module-ref os-module '%os))
