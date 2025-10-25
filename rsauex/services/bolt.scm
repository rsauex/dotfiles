(define-module (rsauex services bolt)
  #:use-module ((gnu packages linux)              #:prefix linux:)
  #:use-module ((gnu services configuration))
  #:use-module ((gnu services dbus)               #:prefix dbus-services:)
  #:use-module ((gnu services shepherd)           #:prefix shepherd-services:)
  #:use-module ((gnu services))
  #:use-module ((guix gexp))
  #:use-module ((ice-9 format))
  #:use-module ((ice-9 match))
  #:use-module ((srfi srfi-1))
  #:use-module ((srfi srfi-171))
  #:use-module ((srfi srfi-26))
  #:use-module ((srfi srfi-34))
  #:use-module ((srfi srfi-35))
  #:export (boltd-configuration
            boltd-configuration-package
            boltd-service-type))

(define-configuration boltd-configuration
  (package
    (file-like linux:bolt)
    "The package providing the @command{boltd} command."
    empty-serializer))

(define (boltd-shepherd-service config)
  (shepherd-services:shepherd-service
   (documentation "Run the Bolt daemon.")
   (provision '(boltd))
   (requirement '(dbus-system))
   (start #~(make-forkexec-constructor
             '(#$(file-append (boltd-configuration-package config) "/libexec/boltd"))
             #:log-file "/var/log/boltd.log"))
   (stop #~(make-kill-destructor))))

(define boltd-service-type
  (service-type
    (name 'boltd)
    (default-value (boltd-configuration))
    (extensions
     (list (service-extension dbus-services:dbus-root-service-type
                              (compose list boltd-configuration-package))
           (service-extension dbus-services:polkit-service-type
                              (compose list boltd-configuration-package))
           (service-extension shepherd-services:shepherd-root-service-type
                              (compose list boltd-shepherd-service))
           (service-extension profile-service-type
                              (compose list boltd-configuration-package))))
    (description "Run @command{boltd}, a userspace daemon for Thunderbolt devices.")))
