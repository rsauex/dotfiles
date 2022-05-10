(define-module (rsauex systems pc master)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system nss)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system pam)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu services base)
  #:use-module (gnu services security-token)
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (%os))

(define %os
  (operating-system
    (host-name "")
    (timezone "Etc/UTC")
    (locale "en_US.utf8")
    (issue "")

    (label "master")
    (bootloader (bootloader-configuration
                 (bootloader (bootloader
                              (inherit grub-efi-bootloader)
                              (installer
                               ;; Copied from (gnu bootloader grub)
                               #~(lambda (bootloader efi-dir mount-point)
                                   ;; There is nothing useful to do when called in the context of a disk
                                   ;; image generation.
                                   (when efi-dir
                                     ;; Install GRUB onto the EFI partition mounted at EFI-DIR, for the
                                     ;; system whose root is mounted at MOUNT-POINT.
                                     (let ((grub-install (string-append bootloader "/sbin/grub-install"))
                                           (install-dir (string-append mount-point "/boot"))
                                           ;; When installing Guix, it's common to mount EFI-DIR below
                                           ;; MOUNT-POINT rather than /boot/efi on the live image.
                                           (target-esp (if (file-exists? (string-append mount-point efi-dir))
                                                           (string-append mount-point efi-dir)
                                                           efi-dir)))
                                       ;; Tell 'grub-install' that there might be a LUKS-encrypted /boot or
                                       ;; root partition.
                                       (setenv "GRUB_ENABLE_CRYPTODISK" "y")
                                       (invoke/quiet grub-install
                                                     "--removable"
                                                     "--boot-directory" install-dir
                                                     "--efi-directory" target-esp)))))))
                 (target "/mnt/boot/efi")))

    ;; (initrd (lambda (file-systems . rest)
    ;;           (apply base-initrd
    ;;                  file-systems
    ;;                  #:volatile-root? #t
    ;;                  rest)))

    (mapped-devices (list
                     (mapped-device
                      (source (uuid "413c889d-432a-4cc9-b3ad-ef9f689fc62c"))
                      (target "cryptroot")
                      (type luks-device-mapping))))

    (file-systems (list
                   (file-system
                     (mount-point "/boot/efi")
                     (device (uuid "D98A-6AEA" 'fat))
                     (type "vfat"))

                   (file-system
                     (mount-point "/")
                     (device "/dev/mapper/cryptroot")
                     (type "ext4")
                     (dependencies mapped-devices))

                   (file-system
                     (mount-point "/tmp")
                     (device "none")
                     (type "tmpfs")
                     (check? #f))

                   %pseudo-terminal-file-system
                   %shared-memory-file-system
                   %efivars-file-system
                   %immutable-store))

    (users (list
            (user-account
             (name "master")
             (group "users")
             (password "")
             (supplementary-groups '("wheel")))))

    (name-service-switch %mdns-host-lookup-nss)

    (pam-services
     ;; Explicitly allow for empty passwords.
     (base-pam-services #:allow-empty-passwords? #t))

    (services (list
               ;; UTF8 terminals
               (service virtual-terminal-service-type)

               ;; Terminals
               (mingetty-service (mingetty-configuration
                                  (tty "tty1")
                                  (auto-login "master")
                                  (login-pause? #f)))
               (mingetty-service (mingetty-configuration
                                  (tty "tty2")
                                  (auto-login "master")
                                  (login-pause? #t)))

               ;; Terminals' fonts
               (service console-font-service-type
                        (map (match-lambda
                               (tty `(,tty . "LatGrkCyr-8x16")))
                             '("tty1" "tty2" ; "tty3" "tty4" "tty5" "tty6"
                               )))

               ;; Terminal mouse support
               (service gpm-service-type)

               ;; Login
               (login-service (login-configuration
                               (motd (plain-file "motd" (format #f "~%")))))

               ;; Might be useful
               (service special-files-service-type
                        `(("/bin/sh" ,(file-append bash "/bin/sh"))))

               ;; UDev
               (service udev-service-type
                        (udev-configuration
                         (rules (list lvm2 fuse))))

               ;; Guix service
               (service guix-service-type
                        (guix-configuration))

               ;; Log
               (syslog-service)

               ;;
               (service urandom-seed-service-type)
               (nscd-service (nscd-configuration
                              (caches (list (nscd-cache (database 'hosts)
                                                        (positive-time-to-live (* 3600 12))

                                                        ;; Do not cache lookup failures at all since they are
                                                        ;; quite likely (for instance when someone tries to ping a
                                                        ;; host before networking is functional.)
                                                        (negative-time-to-live 0)

                                                        (persistent? #f)
                                                        (max-database-size (* 5 (expt 2 20))))))))

               ;; Static network
               (service static-networking-service-type
                        (list (static-networking (interface "lo")
                                                 (ip "127.0.0.1")
                                                 (requirement '())
                                                 (provision '(loopback)))))

               ;; Smart cards support
               (service pcscd-service-type)))

    (packages (append
               (list (@ (gnu packages certs) nss-certs)
                     (@ (gnu packages gnupg) gnupg)
                     (@ (gnu packages gnupg) paperkey)
                     (@ (gnu packages gnupg) pinentry-tty)
                     (@ (rsauex packages powershell) powershell)
                     (@ (gnu packages emacs) emacs)
                     (@ (gnu packages tmux) tmux))
               %base-packages))))
