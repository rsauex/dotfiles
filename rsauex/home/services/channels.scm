(define-module (rsauex home services channels)
  #:use-module ((gnu home services))
  #:use-module ((guix channels))
  #:use-module ((guix gexp))
  #:export (channels-service-type))

(define (add-channels-configuration channels)
  `(("guix/channels.scm"
     ,(scheme-file
       "channels.scm"
       #~(list #$@(map channel->code channels))))))

(define channels-service-type
  (service-type (name 'channels)
                (extensions
                 (list (service-extension
                        home-xdg-configuration-files-service-type
                        add-channels-configuration)))
                (default-value %default-channels)
                (description "List guix channels")))
