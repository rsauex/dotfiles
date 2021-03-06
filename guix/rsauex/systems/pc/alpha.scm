(define-module (rsauex systems pc alpha)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu services xorg)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu packages linux)
  #:use-module (rsauex systems base)
  #:use-module (rsauex systems desktop)
  #:use-module (rsauex packages xorg)
  #:export (%os))

(define %os
  (operating-system
    (inherit %my-base-desktop-system)

    (host-name "alpha")

    (kernel linux)
    (initrd microcode-initrd)
    (firmware (list linux-firmware))

    (mapped-devices (list (mapped-device
                           (source (uuid "eb58fa70-7419-4e29-8d9f-30c53f67cc26"))
                           (target "cryptroot")
                           (type luks-device-mapping))))

    (file-systems (cons* (file-system
                           (device (uuid "1776-7414" 'fat))
                           (mount-point "/boot/efi")
                           (type "vfat"))
                         (file-system
                           (device "/dev/mapper/cryptroot")
                           (mount-point "/")
                           (type "ext4")
                           (flags '(no-atime))
                           (dependencies mapped-devices))
                         %base-file-systems))

    (packages (cons*
               (@ (nongnu packages steam-client) steam)

               ((@ (rsauex packages xorg) my-xorg)
                (xorg-configuration
                 (modules
                  (list (@ (gnu packages xorg) xf86-input-libinput)
                        (@ (gnu packages xorg) xf86-video-intel)
                        (@ (gnu packages xorg) xf86-video-amdgpu)))
                 ))

               (operating-system-packages %my-base-desktop-system)))))
