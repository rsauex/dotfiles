(define-module (rsauex services screen-locker)
  #:use-module (guix records)
  #:use-module (gnu)
  #:use-module (gnu system pam)
  #:use-module (gnu services)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (<my-screen-locker>
            my-screen-locker
            my-screen-locker?
            my-screen-locker-program
            my-screen-locker-pam-service
            my-screen-locker-service-type))

(define-record-type* <my-screen-locker> my-screen-locker
  make-my-screen-locker
  my-screen-locker?
  this-my-screen-locker
  (program     my-screen-locker-program)                  ;gexp
  (pam-service my-screen-locker-pam-service))             ;pam-service

(define my-screen-locker-pam-services
  (compose list my-screen-locker-pam-service))

(define my-screen-locker-setuid-programs
  (compose list my-screen-locker-program))

(define my-screen-locker-service-type
  (service-type (name 'my-screen-locker)
                (extensions
                 (list (service-extension pam-root-service-type
                                          my-screen-locker-pam-services)
                       (service-extension setuid-program-service-type
                                          my-screen-locker-setuid-programs)))
                (description
                 "Allow the given program to be used as a screen locker for
the graphical server by making it setuid-root, so it can authenticate users,
and by creating a PAM service for it.")))
