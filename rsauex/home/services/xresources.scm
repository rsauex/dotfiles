(define-module (rsauex home services xresources)
  #:use-module ((gnu home))
  #:use-module ((gnu home services))
  #:use-module ((gnu packages xorg) #:prefix xorg:)
  #:use-module ((gnu packages))
  #:use-module ((gnu services configuration))
  #:use-module ((gnu services))
  #:use-module ((guix gexp))
  #:use-module ((guix packages))
  #:use-module ((rsauex home services gui-startup) #:prefix my-gui-startup:)
  #:use-module ((rsauex home services shepherd) #:prefix my-shepherd:)
  #:use-module ((srfi srfi-1))
  #:use-module ((ice-9 match))

  #:export (xresources-configuration

            xresources-configuration-xrdb-package
            xresources-configuration-xresources

            xresources-extension

            xresources-extension-xsessions

            xresources-service-type))

(define (xresource? thing)
  (or (file-like? thing)
      (and (pair? thing)
           (string? (car thing))
           (or (string? (cdr thing))
               (integer? (cdr thing))))))

(define (serialize-xresource-value value)
  #~(let ((value #$value))
      (if (integer? value)
          (number->string value)
          value)))

(define (serialize-xresource value)
  (match value
    ((? file-like?)
     #~(string-append "#include \"" #$value "\""))
    ((p-key . p-value)
     #~(string-append #$p-key ":" #$(serialize-xresource-value p-value)))))

(define xresources?
  (list-of xresource?))

(define (serialize-xresources _field-name value)
  #~(string-append
     #$@(interpose
         (map serialize-xresource value)
         "\n" 'suffix)))

(define-configuration xresources-configuration
  (xrdb-package
   (package xorg:xrdb)
   "The xrdb package to use.")
  (xresources
   (xresources '())
   "Xresources"))

(define-configuration/no-serialization xresources-extension
  (xresources
   (xresources '())
   "Xresources"))

(define (xresources-extensions config extensions)
  (xresources-configuration
   (inherit config)
   (xresources (apply append
                      (xresources-configuration-xresources config)
                      (map xresources-extension-xresources extensions)))))

(define (add-xresources-shepherd-service config)
  (let ((xrdb (xresources-configuration-xrdb-package config)))
    (my-gui-startup:gui-startup-extension
     (services
      (list (my-shepherd:simple-one-shot-shepherd-service
             'load-xresources
             "Load xresources."
             #~(lambda ()
                 (invoke #$(file-append xorg:xrdb "/bin/xrdb")
                         "-merge"
                         (string-append "-I" (getenv "HOME"))
                         (string-append (getenv "HOME") "/.xresources"))
                 #t)))))))

(define (add-xresources-files-service config)
  (let ((xresources (xresources-configuration-xresources config)))
    `((".xresources"
       ,(mixed-text-file
         "xresources"
         (serialize-configuration config (filter-configuration-fields
                                          xresources-configuration-fields
                                          '(xresources))))))))

(define xresources-service-type
  (service-type (name 'xresources)
                (extensions
                 (list (service-extension
                        my-gui-startup:gui-startup-service-type
                        add-xresources-shepherd-service)
                       (service-extension
                        home-files-service-type
                        add-xresources-files-service)))
                (compose identity)
                (extend xresources-extensions)
                (default-value (xresources-configuration))
                (description "Load xresources.")))
