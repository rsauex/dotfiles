(define-module (rsauex home services sx)
  #:use-module (gnu home services fontutils)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module ((gnu packages xdisorg) #:prefix xdisorg:)
  #:use-module (gnu services configuration)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix git)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (sx-configuration
            sx-configuration-package
            sx-configuration-sxrc

            sx-extension
            sx-extension-sxrc

            sx-service-type))

(define serialize-gexp empty-serializer)

(define-configuration sx-configuration
  (package
    (package xdisorg:sx)
    "The sx package to use")
  (sxrc
   (gexp #~(begin))
   "Gexp for @file{.config/sx/sxrc}."))

(define-configuration/no-serialization sx-extension
  (sxrc
   (gexp #~(begin))
   "Gexp to execute."))

(define (sx-extensions original-config extension-configs)
  (sx-configuration
   (inherit original-config)
   (sxrc
    #~(begin
        #$(sx-configuration-sxrc original-config)
        #$@(map sx-extension-sxrc extension-configs)))))

(define (sx-files-service config)
  `(("sx/sxrc"
     ,(program-file
       "sxrc"
       (sx-configuration-sxrc config)))))

(define (sx-profile-service config)
  (list (sx-configuration-package config)))

(define sx-service-type
  (service-type (name 'sx)
                (extensions
                 (list (service-extension
                        home-xdg-configuration-files-service-type
                        sx-files-service)
                       (service-extension
                        home-profile-service-type
                        sx-profile-service)))
                (compose identity)
                (extend sx-extensions)
                (default-value (sx-configuration))
                (description "Install and configure sx.")))

;;; Example:

;; (service sx:sx-service-type
;;          (sx:sx-configuration
;;           (sxrc
;;            #~(execl #$(file-append glib:dbus "/bin/dbus-launch")
;;                     #$(file-append glib:dbus "/bin/dbus-launch")
;;                     "--exit-with-session"
;;                     "--"
;;                     #$shepherd-launch
;;                     #$(file-append wm:i3-wm "/bin/i3")))))
