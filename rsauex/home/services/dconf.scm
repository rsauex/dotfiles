(define-module (rsauex home services dconf)
  #:use-module ((gnu home))
  #:use-module ((gnu home services))
  #:use-module ((gnu packages gnome) #:prefix gnome:)
  #:use-module ((gnu packages))
  #:use-module ((gnu services configuration))
  #:use-module ((gnu services))
  #:use-module ((guix gexp))
  #:use-module ((guix packages))
  #:use-module ((rsauex home services gui-startup) #:prefix my-gui-startup:)
  #:use-module ((rsauex home services shepherd) #:prefix my-shepherd:)
  #:use-module ((srfi srfi-1))
  #:use-module ((srfi srfi-197))

  #:export (dconf-configuration
            dconf-configuration-dconf-package
            dconf-configuration-settings

            dconf-extension
            dconf-extension-settings

            dconf-service-type))

(define (setting? thing)
  (and (list? thing)
       (= 3 (length thing))
       (string? (first thing))
       (string? (second thing))
       (or (string? (third thing))
           (integer? (third thing)))))

;; TODO: GVariant format (https://docs.gtk.org/glib/gvariant-format-strings.html)
(define (serialize-setting-value value)
  #~(let ((value #$value))
      (if (integer? value)
          (number->string value)
          (string-append "'" value "'"))))

(define (serialize-setting value)
  #~(string-append #$(second value) "=" #$(serialize-setting-value (third value))))

(define (serialize-settings-group value)
  #~(string-append "[" #$(car value) "]\n" #$@(interpose (map serialize-setting (cdr value)) "\n")))

(define settings?
  (list-of setting?))

(define (serialize-settings _field-name value)
  (let ((groups (make-hash-table)))
    (for-each (lambda (setting)
                (let ((tail (or (hash-ref groups (first setting)) (list))))
                  (hash-set! groups (first setting) (cons setting tail))))
              value)
    #~(string-append
       #$@(interpose
           (map serialize-settings-group (hash-map->list cons groups))
           "\n" 'suffix))))

(define-configuration dconf-configuration
  (dconf-package
   (package gnome:dconf)
   "The dconf package to use.")
  (settings
   (settings '())
   "Settings"))

(define-configuration/no-serialization dconf-extension
  (settings
   (settings '())
   "Settings"))

;; TODO: error on conflicting settings
(define (dconf-extensions config extensions)
  (dconf-configuration
   (inherit config)
   (settings (apply append
                    (dconf-configuration-settings config)
                    (map dconf-extension-settings extensions)))))

(define (add-dconf-env-vars config)
  (my-gui-startup:gui-startup-extension
   (environment
    (list (cons "DCONF_PROFILE" #~(string-append (getenv "XDG_CONFIG_HOME") "/dconf/profile"))))))

(define (add-dconf-files-service config)
  (let ((dconf (dconf-configuration-dconf-package config))
        (settings (dconf-configuration-settings config)))
    `(("dconf/profile"
       ,(mixed-text-file "dconf-profile" "user-db:user\nuser-db:guix-home-db\n"))
      ("dconf/guix-home-db"
       ,(computed-file
         "guix-home-dconf-db"
         (let* ((keyfile (chain (filter-configuration-fields dconf-configuration-fields '(settings))
                                (serialize-configuration config _)
                                (mixed-text-file "guix-home-dconf-keyfile" _)))
                (files (file-union "guix-home-dconf-keyfiles"
                                   `(("keyfile.ini" ,keyfile))))
                (dconf (file-append dconf "/bin/dconf")))
           #~(execl #$dconf #$dconf "compile" #$output #$files)))))))

(define dconf-service-type
  (service-type (name 'dconf)
                (extensions
                 (list (service-extension
                        my-gui-startup:gui-startup-service-type
                        add-dconf-env-vars)
                       (service-extension
                        home-xdg-configuration-files-service-type
                        add-dconf-files-service)))
                (compose identity)
                (extend dconf-extensions)
                (default-value (dconf-configuration))
                (description "Run dconf.")))
