(define-module (rsauex services login)
  #:use-module (guix records)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (<my-login-configuration>
            my-login-configuration
            my-login-configuration?
            my-login-configuration-pam-service
            my-login-service-type))

(define-record-type* <my-login-configuration>
  my-login-configuration make-my-login-configuration
  my-login-configuration?
  (pam-service my-login-configuration-pam-service))  ;pam-service

(define my-login-pam-services
  (compose list my-login-configuration-pam-service))

(define my-login-service-type
  (service-type (name 'my-login)
                (extensions
                 (list (service-extension pam-root-service-type
                                          my-login-pam-services)))
                (description
                 "Provide a console log-in service using pam-service.")))
