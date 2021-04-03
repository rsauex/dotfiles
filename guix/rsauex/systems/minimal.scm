(define-module (rsauex systems minimal)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu services security-token)
  #:use-module (gnu services networking)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu system pam)
  #:use-module (rsauex systems base)
  #:use-module (rsauex services login)
  #:use-module (rsauex services pam-u2f)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (%my-base-minimal-system))

(define %my-base-minimal-system
  (operating-system
    (host-name "")
    (timezone "Europe/Kiev")
    (locale "en_US.UTF-8")

    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (target "/boot/efi")))

    (file-systems %base-file-systems)

    (issue "THIS COMPUTER IS PRIVATE PROPERTY.\n\n")

    (packages (append
               (list
                (@ (gnu packages certs) nss-certs)
                (@ (gnu packages emacs) emacs)
                (@ (gnu packages version-control) git)
                (@ (gnu packages ssh) openssh)

                (@ (gnu packages compression) zip)
                (@ (gnu packages compression) unzip)
                (@ (gnu packages compression) p7zip)

                (@ (gnu packages fonts) font-terminus)

                (@ (gnu packages tmux) tmux)
                (@ (gnu packages package-management) stow)
                (@ (rsauex packages powershell) powershell)
                (@ (gnu packages web) jq)

                (@ (gnu packages security-token) yubikey-personalization)
                (@ (gnu packages security-token) pam-u2f))

               %base-packages-disk-utilities
               %base-packages))

    (services (append
               %my-base-services
               ((compose
                 my-console-font-service-type

                 (cut remove (compose (cut eq? login-service-type <>) service-kind) <>))

                %base-services)))

    (pam-services %my-base-pam-services)

    (users (cons* (user-account
                   (name "rsauex")
                   (uid 1000)
                   (group "users")
                   (supplementary-groups '("audio"
                                           "video"
                                           "input"
                                           "wheel"
                                           "dialout"
                                           "disk")))
                  %base-user-accounts))))
