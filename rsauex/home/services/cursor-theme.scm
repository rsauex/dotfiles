(define-module (rsauex home services cursor-theme)
  #:use-module (gnu home services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:export (cursor-theme-configuration

            cursor-theme-configuration-theme-name
            cursor-theme-configuration-theme-package

            cursor-theme-service-type))

(define-maybe/no-serialization package)
(define-maybe/no-serialization string)

(define-configuration/no-serialization cursor-theme-configuration
  (theme-package
   (maybe-package)
   "The cursor theme package.")
  (theme-name
   (maybe-string)
   "The cursor theme name."))

(define (add-cursor-theme-configuration config)
  (if (or (unspecified? (cursor-theme-configuration-theme-package config))
          (unspecified? (cursor-theme-configuration-theme-name config)))
      `()
      `((".icons/default"
         ,(file-append (cursor-theme-configuration-theme-package config)
                       "/share/icons/"
                       (cursor-theme-configuration-theme-name config))))))

(define cursor-theme-service-type
  (service-type (name 'cursor-theme)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        add-cursor-theme-configuration)))
                (default-value (cursor-theme-configuration))
                (description "Install and set default cursor theme.")))
