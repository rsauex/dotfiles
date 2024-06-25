(define-module (rsauex home services xsettingsd)
  #:use-module ((gnu home))
  #:use-module ((gnu home services))
  #:use-module ((gnu packages xdisorg) #:prefix xdisorg:)
  #:use-module ((gnu packages))
  #:use-module ((gnu services configuration))
  #:use-module ((gnu services))
  #:use-module ((guix gexp))
  #:use-module ((guix packages))
  #:use-module ((rsauex home services gui-startup) #:prefix my-gui-startup:)
  #:use-module ((rsauex home services shepherd) #:prefix my-shepherd:)
  #:use-module ((srfi srfi-1))

  #:export (xsettingsd-configuration

            xsettingsd-configuration-xsettingsd-package
            xsettingsd-configuration-xsessions

            xsettingsd-extension

            xsettingsd-extension-xsessions

            xsettingsd-service-type))

(define (xsetting? thing)
  (and (pair? thing)
       (string? (car thing))
       (or (string? (cdr thing))
           (integer? (cdr thing)))))

(define (serialize-xsetting-value value)
  #~(let ((value #$value))
      (if (integer? value)
          (number->string value)
          (string-append "\"" value "\""))))

(define (serialize-xsetting value)
  #~(string-append #$(car value) " " #$(serialize-xsetting-value (cdr value))))

(define xsettings?
  (list-of xsetting?))

(define (serialize-xsettings _field-name value)
  #~(string-append
     #$@(interpose
         (map serialize-xsetting value)
         "\n" 'suffix)))

(define-configuration xsettingsd-configuration
  (xsettingsd-package
   (package xdisorg:xsettingsd)
   "The xsettingsd package to use.")
  (xsettings
   (xsettings '())
   "Xsettings"))

(define-configuration/no-serialization xsettingsd-extension
  (xsettings
   (xsettings '())
   "Xsettings"))

(define (xsettingsd-extensions config extensions)
  (xsettingsd-configuration
   (inherit config)
   (xsettings (apply append
                     (xsettingsd-configuration-xsettings config)
                     (map xsettingsd-extension-xsettings extensions)))))

(define (add-xsettingsd-shepherd-service config)
  (let ((xsettingsd (xsettingsd-configuration-xsettingsd-package config)))
    (my-gui-startup:gui-startup-extension
     (services
      (list (my-shepherd:simple-forkexec-shepherd-service
             'xsettingsd
             "Run `xsettingsd'"
             #~`(#$(file-append xsettingsd "/bin/xsettingsd"))))))))

(define (add-xsettingsd-files-service config)
  (let ((xsettings (xsettingsd-configuration-xsettings config)))
    `((".xsettingsd"
       ,(mixed-text-file
         "xsettingsd"
         (serialize-configuration config (filter-configuration-fields
                                          xsettingsd-configuration-fields
                                          '(xsettings))))))))

(define xsettingsd-service-type
  (service-type (name 'xsettingsd)
                (extensions
                 (list (service-extension
                        my-gui-startup:gui-startup-service-type
                        add-xsettingsd-shepherd-service)
                       (service-extension
                        home-files-service-type
                        add-xsettingsd-files-service)))
                (compose identity)
                (extend xsettingsd-extensions)
                (default-value (xsettingsd-configuration))
                (description "Run xsettingsd.")))
