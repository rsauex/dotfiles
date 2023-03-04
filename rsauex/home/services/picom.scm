(define-module (rsauex home services picom)
  #:use-module ((gnu home services))
  #:use-module ((gnu packages compton)      #:prefix compton:)
  #:use-module ((gnu services configuration))
  #:use-module ((gnu services))
  #:use-module ((guix gexp))
  #:use-module ((guix packages))
  #:use-module ((rsauex home services gui-startup) #:prefix my-gui-startup:)
  #:use-module ((rsauex home services shepherd)    #:prefix my-shepherd:)
  #:use-module ((srfi srfi-1))
  #:export (picom-configuration

            picom-service-type))

(define-configuration/no-serialization picom-configuration
  (picom
   (package compton:picom)
   "The picom package to use")
  (config
   (file-like)
   "Config file"))

(define (picom-gui-startup-service config)
  (let ((picom (picom-configuration-picom config)))
    (my-gui-startup:gui-startup-extension
     (services
      (list (my-shepherd:simple-forkexec-shepherd-service
             'picom
             "Run `picom' compositor"
             #~`(#$(file-append picom "/bin/picom"))))))))

(define (picom-home-xdg-configuration-files-service config)
  `(("picom/picom.conf"
     ,(picom-configuration-config config))))

(define picom-service-type
  (service-type (name 'picom)
                (extensions
                 (list (service-extension
                        my-gui-startup:gui-startup-service-type
                        picom-gui-startup-service)
                       (service-extension
                        home-xdg-configuration-files-service-type
                        picom-home-xdg-configuration-files-service)))
                (description "Picom startup")))
