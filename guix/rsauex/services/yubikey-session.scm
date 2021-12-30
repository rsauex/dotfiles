(define-module (rsauex services yubikey-session)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (yubikey-session-service-type))

(define yubikey-lock-sessions
  (program-file
   "lock-sessions"
   #~(let ((loginctl #$(file-append (@ (gnu packages freedesktop) elogind)
                                    "/bin/loginctl")))
       (execl loginctl loginctl "lock-sessions"))))

(define (yubikey-session-udev-rule-extension config)
  (list
   (file->udev-rule
    "85-yubikey-session.rules"
    (mixed-text-file
     "yubikey-session.rules"
     "ACTION==\"remove\","
     "ENV{ID_VENDOR_ID}==\"1050\","
     "ENV{ID_MODEL_ID}==\"0407\","
     "RUN+=\"" yubikey-lock-sessions "\""))))

(define yubikey-session-service-type
  (service-type (name 'yubikey-session)
                (extensions
                 (list (service-extension udev-service-type
                                          yubikey-session-udev-rule-extension)))
                (default-value #f)))
