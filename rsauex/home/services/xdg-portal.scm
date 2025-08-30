(define-module (rsauex home services xdg-portal)
  #:use-module ((gnu packages freedesktop)        #:prefix freedesktop:)
  #:use-module ((gnu services configuration))
  #:use-module ((gnu services))
  #:use-module ((guix gexp))
  #:use-module ((guix packages))
  #:use-module ((guix profiles))
  #:use-module ((guix records))
  #:use-module ((rsauex home services gui-startup) #:prefix my-gui-startup:)
  #:use-module ((rsauex home services shepherd)    #:prefix my-shepherd:)
  #:export (xdg-desktop-portal-gtk-backend

            xdg-desktop-portal-configuration

            xdg-desktop-portal-service-type))

(define-configuration/no-serialization xdg-desktop-portal-backend
  (name
   (symbol)
   "Name of the backend")
  (package
    (package)
    "Package of the backend")
  (binary-path
   (string)
   "Path to the executable of the backend"))

(define xdg-desktop-portal-gtk-backend
  (xdg-desktop-portal-backend
   (name 'xdg-desktop-portal-gtk)
   (package freedesktop:xdg-desktop-portal-gtk)
   (binary-path "/libexec/xdg-desktop-portal-gtk")))

(define list-of-xdg-desktop-portal-backends? (list-of xdg-desktop-portal-backend?))

(define-configuration/no-serialization xdg-desktop-portal-configuration
  (xdg-desktop-portal
   (package freedesktop:xdg-desktop-portal)
   "The xdg-desktop-portal package to use")
  (backends
   (list-of-xdg-desktop-portal-backends '())
   "A list of xdg-desktop-portal backends to enable"))

(define (xdg-desktop-portal-backends-profile config)
  (let ((backend-packages (map xdg-desktop-portal-backend-package
                               (xdg-desktop-portal-configuration-backends config))))
    (profile
      (content (packages->manifest backend-packages)))))

(define (xdg-desktop-portal-gui-startup-service config)
  (let ((xdg-desktop-portal (xdg-desktop-portal-configuration-xdg-desktop-portal config))
        (backends-profile (xdg-desktop-portal-backends-profile config)))
    (my-gui-startup:gui-startup-extension
     (services
      (cons* (my-shepherd:simple-forkexec-shepherd-service
              'xdg-desktop-portal
              "Run `xdg-desktop-portal'"
              #~`(#$(file-append xdg-desktop-portal "/libexec/xdg-desktop-portal")))
             (my-shepherd:simple-forkexec-shepherd-service
              'xdg-document-portal
              "Run `xdg-document-portal'"
              #~`(#$(file-append xdg-desktop-portal "/libexec/xdg-document-portal")))
             (my-shepherd:simple-forkexec-shepherd-service
              'xdg-permission-store
              "Run `xdg-permission-store'"
              #~`(#$(file-append xdg-desktop-portal "/libexec/xdg-permission-store")))
             (map (lambda (backend)
                    (let ((name (xdg-desktop-portal-backend-name backend))
                          (package (xdg-desktop-portal-backend-package backend))
                          (binary-path (xdg-desktop-portal-backend-binary-path backend)))
                      (my-shepherd:simple-forkexec-shepherd-service
                       name
                       (string-append "Run `" (symbol->string name) "' xdg-desktop-portal backend")
                       #~`(#$(file-append package binary-path)))))
                  (xdg-desktop-portal-configuration-backends config))))
     (environment
      (list (cons "XDG_DESKTOP_PORTAL_DIR" #~#$(file-append backends-profile "/share/xdg-desktop-portal/portals/")))))))

(define xdg-desktop-portal-service-type
  (service-type (name 'xdg-desktop-portal)
                (extensions
                 (list (service-extension
                        my-gui-startup:gui-startup-service-type
                        xdg-desktop-portal-gui-startup-service)))
                (default-value (xdg-desktop-portal-configuration))
                (description "XDG Portal startup")))
