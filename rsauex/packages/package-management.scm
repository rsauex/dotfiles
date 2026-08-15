(define-module (rsauex packages package-management)
  #:use-module ((gnu packages base)               #:prefix base:)
  #:use-module ((gnu packages freedesktop)        #:prefix freedesktop:)
  #:use-module ((gnu packages gnome)              #:prefix gnome:)
  #:use-module ((gnu packages gtk)                #:prefix gtk:)
  #:use-module ((gnu packages package-management) #:prefix package-management:)
  #:use-module ((gnu))
  #:use-module ((guix)))

(define-public flatpak-fixed
  (package
    (inherit package-management:flatpak)
    (name "flatpak-fixed")
    (arguments
     (substitute-keyword-arguments (package-arguments package-management:flatpak)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'fix-triggers
              (lambda* (#:key inputs #:allow-other-keys)
                ;; desktop-database.trigger
                (wrap-program (string-append #$output "/share/flatpak/triggers/desktop-database.trigger")
                  `("PATH" ":" =
                    (,(string-append #$(this-package-input "desktop-file-utils") "/bin")
                     ,(string-append #$(this-package-input "coreutils") "/bin"))))
                ;; mime-database.trigger
                (wrap-program (string-append #$output "/share/flatpak/triggers/mime-database.trigger")
                  `("PATH" ":" =
                    (,(string-append #$(this-package-input "shared-mime-info") "/bin")
                     ,(string-append #$(this-package-input "coreutils") "/bin"))))
                ;; gtk-icon-cache.trigger
                (substitute* (string-append #$output "/share/flatpak/triggers/gtk-icon-cache.trigger")
                  (("/usr/share/icons/hicolor/index.theme")
                   (string-append #$(this-package-input "hicolor-icon-theme") "/share/icons/hicolor/index.theme")))
                (wrap-program (string-append #$output "/share/flatpak/triggers/gtk-icon-cache.trigger")
                  `("PATH" ":" =
                    (,(dirname (search-input-file inputs "bin/gtk-update-icon-cache"))
                     ,(string-append #$(this-package-input "coreutils") "/bin"))))))))))
    (inputs (modify-inputs (package-inputs package-management:flatpak)
              (append base:coreutils)
              (append freedesktop:desktop-file-utils)
              (append freedesktop:shared-mime-info)
              (append `(,gtk:gtk+ "bin"))
              (append gnome:hicolor-icon-theme)))
    (synopsis (string-append (package-synopsis package-management:flatpak)
                             " (with fixed triggers)"))))
