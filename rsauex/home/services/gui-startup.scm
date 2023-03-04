(define-module (rsauex home services gui-startup)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module ((gnu packages admin)   #:prefix admin:)
  #:use-module ((gnu packages glib)    #:prefix glib:)
  #:use-module ((gnu packages xdisorg) #:prefix xdisorg:)
  #:use-module (guix packages)
  #:use-module ((rsauex home services shepherd) #:prefix my-shepherd:)
  #:use-module ((rsauex home services sx)       #:prefix my-sx:)
  #:use-module (srfi srfi-1)

  #:export (gui-startup-configuration

            gui-startup-configuration?
            gui-startup-configuration-sx-package
            gui-startup-configuration-shepherd-package
            gui-startup-configuration-program
            gui-startup-configuration-services
            gui-startup-configuration-environment

            gui-startup-extension

            gui-startup-extension?
            gui-startup-extension-services
            gui-startup-extension-environment

            gui-startup-service-type))

;;; TODO: GUI -> Desktop ??
;;; TODO: GUI -> WM ??
;;; TODO: GUI -> XOrg ?? (since it's xorg specific and won't work for wayland?)

;;; TOOD: Make it not specific to XOrg ???

;;; TODO: Start DBus as shepherd service

(define (list-of-shepherd-services? x)
  (and (list? x)
       (every shepherd-service? x)))

(define (list-of-environment-pairs? x)
  (and (list? x)
       (every (lambda (p)
                (and (pair? p)
                     (string? (car p))
                     (gexp? (cdr p))))
              x)))

(define-configuration/no-serialization gui-startup-configuration
  (sx-package
   (package xdisorg:sx)
   "The sx package to use")
  (shepherd-package
   (package admin:shepherd-0.9)
   "The shepherd package to use")
  (program
   (file-like (error "Program must be provided"))
   "The program to start (e.g. wm)")
  (services
   (list-of-shepherd-services (list))
   "The list of shepherd services")
  (environment
   (list-of-environment-pairs (list))
   "The list of (<name> . <gexp-value>)"))

(define-configuration/no-serialization gui-startup-extension
  (services
   (list-of-shepherd-services (list))
   "The list of shepherd services")
  (environment
   (list-of-environment-pairs (list))
   "The list of (<name> . <gexp-value>)"))

(define (gui-startup-extensions config extensions)
  (gui-startup-configuration
   (inherit config)
   (services (apply append
                    (gui-startup-configuration-services config)
                    (map gui-startup-extension-services extensions)))
   (environment (apply append
                       (gui-startup-configuration-environment config)
                       (map gui-startup-extension-environment extensions)))))

(define (add-gui-startup-packages config)
  (list (gui-startup-configuration-sx-package config)
        (gui-startup-configuration-shepherd-package config)))

(define (add-sx-configuration-file config)
  (let* ((shepherd-config (my-shepherd:home-shepherd-configuration
                           (shepherd (gui-startup-configuration-shepherd-package config))
                           (services (gui-startup-configuration-services config))))
         (sxrc #~(begin
                   #$@(map (lambda (p)
                             #~(setenv #$(car p) #$(cdr p)))
                           (gui-startup-configuration-environment config))
                   (execl #$(file-append glib:dbus "/bin/dbus-launch")
                          #$(file-append glib:dbus "/bin/dbus-launch")
                          "--exit-with-session"
                          "--"
                          #$(my-shepherd:home-shepherd-session-launch-file shepherd-config)
                          #$(gui-startup-configuration-program config)))))
    `(("sx/sxrc"
       ,(program-file "sxrc" sxrc))
      ("shepherd/shepherd-gui.scm"
       ,(my-shepherd:home-shepherd-configuration-file shepherd-config)))))

(define gui-startup-service-type
  (service-type (name 'guix-startup)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        add-gui-startup-packages)
                       (service-extension
                        home-xdg-configuration-files-service-type
                        add-sx-configuration-file)))
                (compose identity)
                (extend gui-startup-extensions)
                (description "Configurate GUI startup")))
