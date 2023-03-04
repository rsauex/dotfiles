(define-module (rsauex home services dunst)
  #:use-module ((gnu home services))
  #:use-module ((gnu packages dunst)      #:prefix dunst:)
  #:use-module ((gnu services configuration))
  #:use-module ((gnu services))
  #:use-module ((guix gexp))
  #:use-module ((guix packages))
  #:use-module ((rsauex home services gui-startup) #:prefix my-gui-startup:)
  #:use-module ((rsauex home services shepherd)    #:prefix my-shepherd:)
  #:use-module ((srfi srfi-1))
  #:export (dunst-configuration

            dunst-service-type))

(define-configuration/no-serialization dunst-configuration
  (dunst
   (package dunst:dunst)
   "The dunst package to use")
  (config
   (file-like)
   "Config file"))

(define (dunst-gui-startup-service config)
  (let ((dunst (dunst-configuration-dunst config)))
    (my-gui-startup:gui-startup-extension
     (services
      (list (my-shepherd:simple-forkexec-shepherd-service
             'dunst
             "Run `dunst'"
             #~`(#$(file-append dunst "/bin/dunst"))))))))

(define (dunst-home-xdg-configuration-files-service config)
  `(("dunst/dunstrc"
     ,(dunst-configuration-config config))))

(define dunst-service-type
  (service-type (name 'dunst)
                (extensions
                 (list (service-extension
                        my-gui-startup:gui-startup-service-type
                        dunst-gui-startup-service)
                       (service-extension
                        home-xdg-configuration-files-service-type
                        dunst-home-xdg-configuration-files-service)))
                (description "Dunst startup")))
