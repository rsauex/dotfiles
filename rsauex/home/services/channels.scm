(define-module (rsauex home services channels)
  #:use-module (gnu home services)
  #:use-module (gnu packages admin)
  #:use-module (gnu services shepherd)
  #:use-module (guix sets)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module ((guix channels) #:prefix channels:)
  #:use-module (srfi srfi-1)
  #:export (channels-service-type))

(define (add-channels-configuration channels)
  `(("guix/channels.scm"
     ,(scheme-file
       "channels.scm"
       #~(list #$@(map channels:channel->code channels))))))

(define channels-service-type
  (service-type (name 'channels)
                (extensions
                 (list (service-extension
                        home-xdg-configuration-files-service-type
                        add-channels-configuration)))
                (default-value channels:%default-channels)
                (description "List guix channels")))
