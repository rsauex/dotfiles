(define-module (rsauex systems base)
  #:use-module ((gnu packages certs)              #:prefix certs:)
  #:use-module ((gnu packages fonts)              #:prefix fonts:)
  #:use-module ((gnu packages security-token)     #:prefix security-token:)
  #:use-module ((gnu services networking)         #:prefix network-services:)
  #:use-module ((gnu services security-token)     #:prefix security-token-services:)
  #:use-module ((gnu))
  #:use-module ((ice-9 match))
  #:use-module ((srfi srfi-1))
  #:export (%my-base-services
            %my-base-packages
            %my-base-system))

(define %my-base-packages
  (cons*
   certs:nss-certs
   (append %base-packages-disk-utilities
           %base-packages)))

;; TODO: Better name!
(define (my-console-font-service-type services)
  (modify-services services
    (console-font-service-type
     config =>
     (let ((new-font (file-append fonts:font-terminus
                                  "/share/consolefonts/ter-v16n.psf.gz")))
       (map (match-lambda
              ((tty . font)
               `(,tty . ,new-font)))
            config)))))

(define %my-base-services
  (cons*
   ;; Enable PC/SC smart card daemon
   (service security-token-services:pcscd-service-type)

   ;; Let users access the Yubikey USB device node
   (udev-rules-service 'yubikey-udev security-token:yubikey-personalization)

   ;; NTP
   (service network-services:ntp-service-type
            (network-services:ntp-configuration
             (allow-large-adjustment? #t)
             (servers (map (lambda (address)
                             (network-services:ntp-server
                              (type 'pool)
                              (address address)
                              (options '("iburst"))))
                           (list "0.europe.pool.ntp.org"
                                 "1.europe.pool.ntp.org"
                                 "2.europe.pool.ntp.org"
                                 "3.europe.pool.ntp.org")))))

   ((compose my-console-font-service-type)
    %base-services)))

(define %my-base-system
  (operating-system
    (host-name "")
    (timezone "Europe/Kiev")
    (locale "en_IE.UTF-8")

    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets (list "/boot/efi"))))

    (file-systems %base-file-systems)

    (issue "THIS COMPUTER IS PRIVATE PROPERTY.\n\n")

    (packages %my-base-packages)

    (services %my-base-services)

    (users (cons* (user-account
                   (name "rsauex")
                   (uid 1000)
                   (group "users")
                   (supplementary-groups '("audio"
                                           "video"
                                           "input"
                                           "wheel"
                                           "dialout"
                                           "disk"
                                           "kvm"
                                           "lpadmin"
                                           "lp"
                                           "docker")))
                  %base-user-accounts))))
