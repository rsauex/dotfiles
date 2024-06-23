(define-module (rsauex systems pc gamma)
  #:use-module ((gnu services pm)                 #:prefix pm-services:)
  #:use-module ((gnu services xorg)               #:prefix xorg-services:)
  #:use-module ((gnu system pam)                  #:prefix system-pam:)
  #:use-module ((gnu))
  #:use-module ((guix))
  #:use-module ((nongnu packages linux)           #:prefix non-linux:)
  #:use-module ((nongnu packages nvidia)          #:prefix non-nvidia:)
  #:use-module ((nongnu packages video)           #:prefix non-video:)
  #:use-module ((nongnu services nvidia)          #:prefix non-nvidia-services:)
  #:use-module ((nongnu system linux-initrd)      #:prefix non-linux-initrd:)
  #:use-module ((rsauex systems desktop)          #:prefix my-desktop-systems:)
  #:export (%os))

(define gamma-xorg-config
  "Section \"Monitor\"
       Identifier  \"eDP-1\"
       DisplaySize 410 231
       # DisplaySize 344 194 # This is the correct one
   EndSection

   Section \"Device\"
       Identifier  \"Intel Card\"
       Driver      \"modesetting\"
       BusID       \"PCI:0:2:0\"
       # Option      \"Monitor-eDP-1\" \"eDP-1\"
   EndSection

  Section \"Device\"
       Identifier \"Nvidia Card\"
       Driver     \"nvidia\"
       VendorName \"NVIDIA Corporation\"
       BusID      \"PCI:1:0:0\"
       Option     \"SidebandSocketPath\" \"/tmp\"
  EndSection

  Section \"Screen\"
       Identifier \"Intel Screen\"
       Device     \"Intel Card\"
  EndSection

  Section \"Screen\"
       Identifier \"Nvidia Screen\"
       Device     \"Nvidia Card\"
  EndSection

  # Section \"ServerLayout\"
  #      Identifier \"Main Layout\"
  #      Screen     0 \"Intel Screen\"
  #      Inactive   \"Nvidia Card\"
  #      Option     \"AllowNVIDIAGPUScreens\"
  # EndSection

  Section \"ServerLayout\"
       Identifier \"Main Layout\"
       Screen     0 \"Nvidia Screen\"
  EndSection
")

(define kernel-args-nvidia
  (list "modprobe.blacklist=nouveau"
        "ibt=off" ;; See https://github.com/NVIDIA/open-gpu-kernel-modules/issues/256
        ))

(define (tlp-service)
  (let ((config (pm-services:tlp-configuration
                 (tlp-enable? #t)
                 (sound-power-save-on-ac 1)
                 (sound-power-save-on-bat 1)
                 (sound-power-save-controller? #t)
                 (start-charge-thresh-bat0 75)
                 (stop-charge-thresh-bat0 85)
                 (sata-linkpwr-on-ac "med_power_with_dipm")
                 (sata-linkpwr-on-bat "med_power_with_dipm")
                 (nmi-watchdog? #f)
                 (wifi-pwr-on-ac? #f)
                 (wifi-pwr-on-bat? #f)
                 (wol-disable? #t)
                 (cpu-scaling-governor-on-ac '("powersave"))
                 (cpu-scaling-governor-on-bat '("powersave"))
                 (cpu-min-perf-on-ac 17)
                 (cpu-max-perf-on-ac 100)
                 (cpu-min-perf-on-bat 17)
                 (cpu-max-perf-on-bat 100)
                 (cpu-boost-on-ac? #f)
                 (cpu-boost-on-bat? #f)
                 (sched-powersave-on-ac? #t)
                 (sched-powersave-on-bat? #t)
                 (runtime-pm-on-ac "auto")
                 (runtime-pm-on-bat "auto")
                 (runtime-pm-blacklist '("0b:00.0"))
                 (pcie-aspm-on-ac "default")
                 (pcie-aspm-on-bat "default")
                 (usb-autosuspend? #t))))
    (service pm-services:tlp-service-type config)))

(define %os
  (operating-system
    (inherit my-desktop-systems:%my-base-desktop-system)

    (host-name "gamma")

    (kernel non-linux:linux-lts)
    (kernel-arguments (append kernel-args-nvidia
                              (operating-system-user-kernel-arguments my-desktop-systems:%my-base-desktop-system)))
    (initrd non-linux-initrd:microcode-initrd)
    (firmware (list non-linux:linux-firmware
                    non-linux:sof-firmware))

    (mapped-devices (list (mapped-device
                           (source (uuid "303dd691-56d1-40e9-8992-4cfd1627e0ec"))
                           (target "cryptroot")
                           (type luks-device-mapping))))

    (file-systems (cons* (file-system
                           (device (uuid "8B75-092F" 'fat))
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

    (packages (cons* (operating-system-packages my-desktop-systems:%my-base-desktop-system)))

    (services (cons* (tlp-service)

                     ;; TODO: what if I want to use nvidia for hardware acceleration?
                     (simple-service 'intel-media-driver-va
                                     system-pam:session-environment-service-type
                                     `(("LIBVA_DRIVERS_PATH"
                                        . ,(file-append non-video:intel-media-driver "/lib/dri"))
                                       ("LIBVA_DRIVER_NAME"
                                        . "iHD")))
                     (simple-service 'host-dpi-env-var
                                     system-pam:session-environment-service-type
                                     `(("HOST_DPI"
                                        . "119")))

                     (service non-nvidia-services:nvidia-service-type)

                     (modify-services
                         (operating-system-user-services my-desktop-systems:%my-base-desktop-system)
                       (xorg-services:xorg-server-service-type
                        config => (xorg-services:xorg-configuration
                                   (inherit config)
                                   (modules
                                    (cons* non-nvidia:nvda
                                           (xorg-services:xorg-configuration-modules config)))
                                   (extra-config
                                    (cons* gamma-xorg-config
                                           (xorg-services:xorg-configuration-extra-config config))))))))))
