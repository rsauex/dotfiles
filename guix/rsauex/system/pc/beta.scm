(define-module (rsauex system pc beta)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu packages linux)
  #:use-module (rsauex system base-desktop-system)
  #:export (%os))

(define %os
  (operating-system
    (inherit %base-desktop-system)

    (host-name "beta")
    
    (kernel linux)
    (initrd microcode-initrd)
    (firmware (list linux-firmware))

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

    (swap-devices '("/swapfile"))))
