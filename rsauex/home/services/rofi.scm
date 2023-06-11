(define-module (rsauex home services rofi)
  #:use-module ((gnu home services))
  #:use-module ((gnu packages xdisorg) #:prefix xdisorg:)
  #:use-module ((gnu services configuration))
  #:use-module ((gnu services))
  #:use-module ((guix gexp))
  #:use-module ((guix packages))
  #:use-module ((ice-9 match))

  #:export (rofi-configuration

            rofi-configuration-package
            rofi-configuration-config
            rofi-configuration-theme

            rofi-service-type))

(define (rofi-serialize-config field-name val)
  #~(begin
      (use-modules (ice-9 string-fun))

      (let ((serialize-value
             (lambda (value)
               (cond
                ((string? value)
                 (string-append "\"" (string-replace-substring value "\"" "\\\"") "\""))
                ((number? value)
                 (number->string value))
                (#t
                 (raise "Only string and number values are supported currently"))))))
        (string-append
         "configuration {\n"
         #$@(map (match-lambda
                   ((key . value)
                    #~(string-append #$key ": " (serialize-value #$value) ";\n")))
                 val)
         "}\n"))))

(define (rofi-serialize-theme field-name val)
  #~(let ((val #$val))
      (if (unspecified? val)
          ""
          (string-append "@theme \"" #$val "\"\n"))))

(define-maybe file-like)

(define-configuration rofi-configuration
  (package
    (package xdisorg:rofi)
    "The Rofi package to use.")
  (config
   (alist '())
   "Association list of configuration options."
   (serializer rofi-serialize-config))
  (theme
   (maybe-file-like)
   "File-like with theme."
   (serializer rofi-serialize-theme)))

(define (add-rofi-configuration config)
  `(("rofi/config.rasi"
     ,(mixed-text-file
       "config_rasi"
       (rofi-serialize-config 'config (rofi-configuration-config config))
       (rofi-serialize-theme 'theme (rofi-configuration-theme config))))))

(define (add-rofi-package config)
  (list (rofi-configuration-package config)))

(define rofi-service-type
  (service-type (name 'rofi)
                (extensions
                 (list (service-extension
                        home-xdg-configuration-files-service-type
                        add-rofi-configuration)
                       (service-extension
                        home-profile-service-type
                        add-rofi-package)))
                (default-value (rofi-configuration))
                (description "Install and configure Rofi.")))
