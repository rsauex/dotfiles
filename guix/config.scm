(use-modules (gnu)
             (gnu packages)
             (gnu services networking)
             (nongnu system linux-initrd)
             (nongnu packages linux)
             (ice-9 match)
             (rsauex packages powershell))

(operating-system
  (host-name "x240")
  (timezone "Europe/Kiev")
  (locale "en_US.UTF-8")

  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))

  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (target "/boot/efi")))

  (mapped-devices (list (mapped-device
                         (source "/dev/sda2")
                         (target "cryptroot")
                         (type luks-device-mapping))))

  (file-systems (cons* (file-system
                        (device "/dev/mapper/cryptroot")
                        (mount-point "/")
                        (type "ext4")
                        (flags '(no-atime))
                        (dependencies mapped-devices))
                       (file-system
                        (device "/dev/sda1")
                        (mount-point "/boot/efi")
                        (type "vfat"))
                       %base-file-systems))

  (packages (cons* (specification->package "nss-certs")
                   (specification->package "emacs-no-x")
                   (specification->package "git")
                   (specification->package "openssh")
                   (specification->package "font-terminus")
                   (specification->package "tmux")
                   (specification->package "xinit")
                   (specification->package "xorg-server")
                   (specification->package "xf86-input-libinput")
                   (specification->package "xf86-video-vesa")
                   (specification->package "alacritty")
                   (specification->package "strace")
                   powershell
                   %base-packages))

  (services (cons* (service dhcp-client-service-type)
                   (modify-services %base-services
                     (console-font-service-type configuration =>
                       (map (match-lambda
                              ((tty . font) `(,tty . ,(file-append (specification->package "font-terminus")
                                                                   "/share/consolefonts/ter-v16n.psf.gz"))))
                            configuration)))))

  (users (cons* (user-account
                 (name "rsauex")
                 (uid 1000)
                 (group "users")
                 (supplementary-groups '("audio"
                                         "video"
                                         "input"
                                         "wheel")))
                %base-user-accounts))
  )
