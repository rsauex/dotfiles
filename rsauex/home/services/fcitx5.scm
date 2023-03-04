(define-module (rsauex home services fcitx5)
  #:use-module ((gnu home services))
  #:use-module ((gnu packages fcitx5)              #:prefix fcitx5:)
  #:use-module ((gnu services configuration))
  #:use-module ((gnu services))
  #:use-module ((guix gexp))
  #:use-module ((guix packages))
  #:use-module ((rsauex home services gui-startup) #:prefix my-gui-startup:)
  #:use-module ((rsauex home services shepherd)    #:prefix my-shepherd:)

  #:export (fcitx5-configuration

            fcitx5-service-type))

(define packages?
  (list-of (const #t)))

(define-configuration/no-serialization fcitx5-configuration
  (fcitx5
   (package fcitx5:fcitx5)
   "The Fcitx5 package to use.")
  (fcitx5-configtool
   (package fcitx5:fcitx5-configtool)
   "The Fcitx5 configtool package to use.")
  (modules
   (packages (list fcitx5:fcitx5-qt
                   (list fcitx5:fcitx5-gtk "gtk2")
                   (list fcitx5:fcitx5-gtk "gtk3")))
   "The Fcitx5 modules to enable.")
  (addons
   (packages (list))
   "Extra input methods"))

(define (fcitx5-home-profile-service config)
  (append (list (fcitx5-configuration-fcitx5 config)
                (fcitx5-configuration-fcitx5-configtool config))
          (fcitx5-configuration-modules config)
          (fcitx5-configuration-addons config)))

(define (fcitx5-gui-startup-service config)
  (let ((fcitx5 (fcitx5-configuration-fcitx5 config)))
    (my-gui-startup:gui-startup-extension
     (services
      (list (my-shepherd:simple-forkexec-shepherd-service
             'fcitx5
             "Run `fcixt5'"
             #~`(#$(file-append fcitx5 "/bin/fcitx5")))))
     (environment
      (list (cons "GTK_IM_MODULE" #~"fcitx")
            (cons "QT_IM_MODULE" #~"fcitx")
            (cons "XMODIFIERS" #~"@im=fcitx"))))))

(define fcitx5-service-type
  (service-type (name 'fcitx5)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        fcitx5-home-profile-service)
                       (service-extension
                        my-gui-startup:gui-startup-service-type
                        fcitx5-gui-startup-service)))
                (default-value (fcitx5-configuration))
                (description "Configure Fcitx5.")))
