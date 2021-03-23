(define-module (rsauex services pam-u2f)
  #:use-module (guix gexp)
  #:use-module (gnu)
  #:use-module (gnu system pam)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (pam-u2f-entry))

(define* (pam-u2f-entry control #:rest args)
  (pam-entry (control "sufficient")
             (module (file-append (@ (gnu packages security-token) pam-u2f)
                                  "/lib/security/pam_u2f.so"))
             (arguments args)))
