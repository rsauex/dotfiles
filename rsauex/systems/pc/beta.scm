(define-module (rsauex systems pc beta)
  #:use-module ((gnu services pm)                 #:prefix pm-services:)
  #:use-module ((gnu services xorg)               #:prefix xorg-services:)
  #:use-module ((gnu system linux-initrd)         #:prefix linux-initrd:)
  #:use-module ((gnu))
  #:use-module ((guix))
  #:use-module ((nongnu packages linux)           #:prefix non-linux:)
  #:use-module ((nongnu system linux-initrd)      #:prefix non-linux-initrd:)
  #:use-module ((rsauex services intel-backlight) #:prefix my-intel-backlight:)
  #:use-module ((rsauex systems desktop)          #:prefix my-desktop-systems:)
  #:export (%os))

(define xorg-enable-dri3
  "Section \"Monitor\"
       Identifier  \"eDP1\"
       # DisplaySize 276 155
   EndSection

   Section \"Device\"
       Identifier  \"Intel Graphics\"
       Driver      \"modesetting\"
       Option      \"AccelMethod\" \"glamor\"
       Option      \"DRI\"  \"3\"
   EndSection")

(define (tlp-service)
  (let ((config (pm-services:tlp-configuration
                 (tlp-enable? #t)
                 (sound-power-save-on-ac 1)
                 (sound-power-save-on-bat 1)
                 (sound-power-save-controller? #t)
                 (start-charge-thresh-bat0 75)
                 (stop-charge-thresh-bat0 85)
                 (start-charge-thresh-bat1 75)
                 (stop-charge-thresh-bat1 85)
                 (disks-devices '()) ;; it should be disk-devices not disks-devices
                 (sata-linkpwr-on-ac "med_power_with_dipm")
                 (sata-linkpwr-on-bat "med_power_with_dipm")
                 (nmi-watchdog? #f)
                 (wifi-pwr-on-ac? #f)
                 (wifi-pwr-on-bat? #f)
                 (wol-disable? #t)
                 (cpu-scaling-governor-on-ac '("schedutil"))
                 (cpu-scaling-governor-on-bat '("schedutil"))
                 (cpu-min-perf-on-ac 27)
                 (cpu-max-perf-on-ac 100)
                 (cpu-min-perf-on-bat 27)
                 (cpu-max-perf-on-bat 100)
                 (cpu-boost-on-ac? #t)
                 (cpu-boost-on-bat? #f)
                 (sched-powersave-on-ac? #f)
                 (sched-powersave-on-bat? #t)
                 (runtime-pm-on-ac "on")
                 (runtime-pm-on-bat "auto")
                 (pcie-aspm-on-ac "default")
                 (pcie-aspm-on-bat "default")
                 (usb-autosuspend? #t))))
    (service pm-services:tlp-service-type config)))

(define %os
  (operating-system
    (inherit my-desktop-systems:%my-base-desktop-system)

    (host-name "beta")

    (kernel non-linux:linux)
    (kernel-arguments (cons* "resume=/dev/mapper/cryptroot"
                             "resume_offset=3315712"
                             (operating-system-user-kernel-arguments my-desktop-systems:%my-base-desktop-system)))
    (initrd non-linux-initrd:microcode-initrd)
    (firmware (list non-linux:linux-firmware))

    (mapped-devices (list (mapped-device
                           (source (uuid "1caaeb4e-d8a9-45e0-a2ea-6b84ec9478e8"))
                           (target "cryptroot")
                           (type luks-device-mapping))))

    (file-systems (cons* (file-system
                           (device (uuid "3A7B-4B26" 'fat))
                           (mount-point "/boot/efi")
                           (type "vfat"))
                         (file-system
                           (device "/dev/mapper/cryptroot")
                           (mount-point "/")
                           (type "ext4")
                           (flags '(no-atime))
                           (dependencies mapped-devices))
                         (operating-system-file-systems my-desktop-systems:%my-base-desktop-system)))

    (swap-devices (list (swap-space
                         (target "/swapfile"))))

    (packages (cons* (@ (gnu packages video) intel-vaapi-driver)
                     (operating-system-packages my-desktop-systems:%my-base-desktop-system)))

    (services (cons* (my-intel-backlight:intel-backlight-service)
                     (tlp-service)
                     (service pm-services:thermald-service-type)
                     (modify-services
                         (operating-system-user-services my-desktop-systems:%my-base-desktop-system)
                       (xorg-services:xorg-server-service-type
                        config => (xorg-services:xorg-configuration
                                   (inherit config)
                                   (extra-config
                                    (cons* xorg-enable-dri3
                                           (xorg-services:xorg-configuration-extra-config config))))))))))
