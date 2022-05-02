(define-module (rsauex systems minimal)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module ((gnu packages admin)              #:prefix admin:)
  #:use-module ((gnu packages certs)              #:prefix certs:)
  #:use-module ((gnu packages compression)        #:prefix compression:)
  #:use-module ((gnu packages emacs)              #:prefix emacs:)
  #:use-module ((gnu packages fonts)              #:prefix fonts:)
  #:use-module ((gnu packages package-management) #:prefix package-management:)
  #:use-module ((gnu packages security-token)     #:prefix security-token:)
  #:use-module ((gnu packages ssh)                #:prefix ssh:)
  #:use-module ((gnu packages tmux)               #:prefix tmux:)
  #:use-module ((gnu packages version-control)    #:prefix vc:)
  #:use-module ((gnu packages web)                #:prefix web:)
  #:use-module ((rsauex packages powershell)      #:prefix powershell:)
  #:use-module (rsauex systems base)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%my-base-minimal-system))

(define %my-base-minimal-system
  (operating-system
    (host-name "")
    (timezone "Europe/Kiev")
    (locale "en_US.UTF-8")

    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets (list "/boot/efi"))))

    (file-systems %base-file-systems)

    (issue "THIS COMPUTER IS PRIVATE PROPERTY.\n\n")

    (packages (append
               (list
                certs:nss-certs
                vc:git
                ssh:openssh

                compression:zip
                compression:unzip
                compression:p7zip

                fonts:font-terminus

                admin:htop
                tmux:tmux
                emacs:emacs
                package-management:stow
                powershell:powershell
                web:jq

                security-token:yubikey-personalization
                security-token:pam-u2f)

               %base-packages-disk-utilities
               %base-packages))

    (services (append
               %my-base-services
               ((compose
                 my-console-font-service-type)

                %base-services)))

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
                                           "lpadmin"
                                           "lp"
                                           "docker")))
                  %base-user-accounts))))
