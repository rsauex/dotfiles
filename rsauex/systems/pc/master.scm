(define-module (rsauex systems pc master)
  #:use-module ((gnu packages bash)            #:prefix bash:)
  #:use-module ((gnu packages linux)           #:prefix linux:)
  #:use-module ((gnu services base)            #:prefix base-services:)
  #:use-module ((gnu services security-token)  #:prefix security-token-services:)
  #:use-module ((gnu))
  #:use-module ((guix))
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
                  (targets (list "/mnt/boot/efi"))))

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
               (service base-services:virtual-terminal-service-type)

               ;; Terminals
               (service base-services:mingetty-service-type
                        (base-services:mingetty-configuration
                         (tty "tty1")
                         (auto-login "master")
                         (login-pause? #f)))
               (service base-services:mingetty-service-type
                        (base-services:mingetty-configuration
                         (tty "tty2")
                         (auto-login "master")
                         (login-pause? #t)))

               ;; Terminals' fonts
               (service base-services:console-font-service-type
                        (map (lambda (tty)
                               (tty `(,tty . "LatGrkCyr-8x16")))
                             '("tty1" "tty2" ; "tty3" "tty4" "tty5" "tty6"
                               )))

               ;; Terminal mouse support
               (service base-services:gpm-service-type)

               ;; Login
               (service base-services:login-service-type
                        (base-services:login-configuration
                         (motd (plain-file "motd" (format #f "~%")))))

               ;; Might be useful
               (service special-files-service-type
                        `(("/bin/sh" ,(file-append bash:bash "/bin/sh"))))

               ;; UDev
               (service base-services:udev-service-type
                        (base-services:udev-configuration
                         (rules (list linux:lvm2 linux:fuse))))

               ;; Guix service
               (service base-services:guix-service-type
                        (base-services:guix-configuration))

               ;; Log
               (service base-services:syslog-service-type)

               ;;
               (service base-services:urandom-seed-service-type)
               (service base-services:nscd-service-type
                        (base-services:nscd-configuration
                         (caches (list (base-services:nscd-cache
                                        (database 'hosts)
                                        (positive-time-to-live (* 3600 12))

                                        ;; Do not cache lookup failures at all since they are
                                        ;; quite likely (for instance when someone tries to ping a
                                        ;; host before networking is functional.)
                                        (negative-time-to-live 0)

                                        (persistent? #f)
                                        (max-database-size (* 5 (expt 2 20))))))))

               ;; Static network
               (service base-services:static-networking-service-type
                        base-services:%loopback-static-networking)

               ;; Smart cards support
               (service security-token-services:pcscd-service-type)))

    (packages (append
               (list (@ (gnu packages nss) nss-certs)
                     (@ (gnu packages gnupg) gnupg)
                     (@ (gnu packages gnupg) paperkey)
                     (@ (gnu packages gnupg) pinentry-tty)
                     (@ (rsauex packages powershell) powershell)
                     (@ (gnu packages emacs) emacs)
                     (@ (gnu packages tmux) tmux))
               %base-packages))))
