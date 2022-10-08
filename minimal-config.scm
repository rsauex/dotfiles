(use-modules (gnu)
             (gnu packages)
             (gnu services networking)
             (gnu services desktop)
             (gnu services xorg)
             (nongnu system linux-initrd)
             (nongnu packages linux)
             (rsauex system base)
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
  (inherit %my-base-system)

  (host-name (read-line-non-empty "Enter host name: "))

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
                       %base-file-systems)))
