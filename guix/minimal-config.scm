(use-modules (gnu)
             (gnu packages)
             (gnu services networking)
             (gnu services desktop)
             (gnu services xorg)
             (nongnu system linux-initrd)
             (nongnu packages linux)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match)
             (ice-9 readline))

(define (read-line-non-empty prompt)
  (let ((result (readline prompt)))
    (if (string-null? result)
        (begin
          (display "Please, enter non empty value!")
          (newline)
          (read-line-non-empty prompt))
        result)))

(operating-system
  (host-name (read-line-non-empty "Enter host name: "))
  (timezone "Europe/Kiev")
  (locale "en_US.UTF-8")

  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (target "/boot/efi")))

  (mapped-devices (list (mapped-device
                         (source "/dev/sda2")
                         (target "cryptroot")
                         (type luks-device-mapping))))

  (file-systems (cons* (file-system
                         (device "/dev/sda1")
                         (mount-point "/boot/efi")
                         (type "vfat"))
                       (file-system
                         (device "/dev/mapper/cryptroot")
                         (mount-point "/")
                         (type "ext4")
                         (flags '(no-atime))
                         (dependencies mapped-devices))
                       %base-file-systems))

  (issue "THIS COMPUTER IS PRIVATE PROPERTY.\n\n")

  (packages (cons* (@ (gnu packages certs) nss-certs)
                   (@ (gnu packages emacs) emacs)
                   (@ (gnu packages version-control) git)
                   (@ (gnu packages ssh) openssh)

                   (@ (gnu packages package-management) stow)
                   (@ (rsauex packages powershell) powershell)

                   %base-packages))

  (services (cons*
             ((compose
               (lambda (services)
                 (modify-services services
                   (console-font-service-type
                    config =>
                    (map (match-lambda
                           ((tty . font)
                            `(,tty . ,(file-append (specification->package "font-terminus")
                                                   "/share/consolefonts/ter-v16n.psf.gz"))))
                         config))
                   (ntp-service-type
                    config =>
                    (ntp-configuration
                     (inherit config)
                     (allow-large-adjustment? #t))))))
              %base-services)))

  (users (cons* (user-account
                 (name "rsauex")
                 (uid 1000)
                 (group "users")
                 (supplementary-groups '("audio"
                                         "video"
                                         "input"
                                         "wheel")))
                %base-user-accounts)))
