(define-module (rsauex home services fcitx5)
  #:use-module ((gnu home services))
  #:use-module ((gnu packages fcitx5)              #:prefix fcitx5:)
  #:use-module ((gnu services configuration))
  #:use-module ((gnu services))
  #:use-module ((guix gexp))
  #:use-module ((guix packages))
  #:use-module ((guix profiles))
  #:use-module ((ice-9 match))
  #:use-module ((rsauex home services gui-startup) #:prefix my-gui-startup:)
  #:use-module ((rsauex home services shepherd)    #:prefix my-shepherd:)
  #:use-module ((srfi srfi-1))

  #:export (fcitx5-configuration

            fcitx5-service-type))

(define package-with-maybe-output?
  (match-lambda
    ((? package? pkg) #t)
    (((? package? pkg) output) #t)
    (_ #f)))

(define-configuration/no-serialization fcitx5-configuration
  (fcitx5
   (package fcitx5:fcitx5)
   "The Fcitx5 package to use.")
  (fcitx5-configtool
   (package fcitx5:fcitx5-configtool)
   "The Fcitx5 configtool package to use.")
  (gtk2-module
   (package-with-maybe-output (list fcitx5:fcitx5-gtk "gtk2"))
   "The Fcitx5 GTK2 module.")
  (gtk3-module
   (package-with-maybe-output (list fcitx5:fcitx5-gtk "gtk3"))
   "The Fcitx5 GTK3 module.")
  (qt-module
   (package-with-maybe-output fcitx5:fcitx5-qt)
   "The Fcitx5 QT module.")
  (addons
   (list-of-packages (list))
   "Extra input methods"))

(define (fcitx5-home-profile-service config)
  (list (fcitx5-configuration-fcitx5-configtool config)))

;; KLUDGE: wrapping module packages in profiles forces generation of
;; immodules-gtk*.cache files
(define (module-profile package)
  (profile
    (content (packages->manifest (list package)))))

(define (fcitx5-gui-startup-service config)
  (let* ((fcitx5 (fcitx5-configuration-fcitx5 config))
         (addons (cons fcitx5 (fcitx5-configuration-addons config)))
         (gtk2-module-profile (module-profile (fcitx5-configuration-gtk2-module config)))
         (gtk3-module-profile (module-profile (fcitx5-configuration-gtk3-module config)))
         (qt-module-profile (module-profile (fcitx5-configuration-qt-module config))))
    (my-gui-startup:gui-startup-extension
     (services
      (list (my-shepherd:simple-forkexec-shepherd-service
             'fcitx5
             "Run `fcixt5'"
             #~`(#$(file-append fcitx5 "/bin/fcitx5"))
             #:extra-environment-variables
             (list #~(string-append "FCITX_ADDON_DIRS="
                                    (string-join '#$(map (lambda (addon) (file-append addon "/lib/fcitx5")) addons)
                                                 ":"))
                   #~(string-append "FCITX_DATA_DIRS="
                                    (string-join '#$(map (lambda (addon) (file-append addon "/share/fcitx5")) addons)
                                                 ":"))))))
     (environment
      (list
       ;; GTK
       (cons "GUIX_GTK2_IM_MODULE_FILE" #~(string-append #$gtk2-module-profile "/lib/gtk-2.0/2.10.0/immodules-gtk2.cache"))
       (cons "GUIX_GTK3_IM_MODULE_FILE" #~(string-append #$gtk3-module-profile "/lib/gtk-3.0/3.0.0/immodules-gtk3.cache"))
       (cons "GTK_IM_MODULE" #~"fcitx")
       ;; QT
       (cons "QT_PLUGIN_PATH" #~(let ((qt-plugin-paths (list (string-append #$qt-module-profile "/lib/qt5/plugins")
                                                             (string-append #$qt-module-profile "/lib/qt6/plugins")))
                                      (prev-value (getenv "QT_PLUGIN_PATH")))
                                  (if prev-value
                                      (string-append prev-value ":" (string-join qt-plugin-paths ":"))
                                      (string-join qt-plugin-paths ":"))))
       (cons "QT_IM_MODULE" #~"fcitx")
       ;; XIM
       (cons "XMODIFIERS" #~"@im=fcitx"))))))

(define (fcitx5-extensions config addons)
  (fcitx5-configuration
   (inherit config)
   (addons (append (fcitx5-configuration-addons config) addons))))

(define fcitx5-service-type
  (service-type (name 'fcitx5)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        fcitx5-home-profile-service)
                       (service-extension
                        my-gui-startup:gui-startup-service-type
                        fcitx5-gui-startup-service)))
                (compose concatenate)
                (extend fcitx5-extensions)
                (default-value (fcitx5-configuration))
                (description "Configure Fcitx5.")))
