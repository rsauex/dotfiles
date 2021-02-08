(define-module (rsauex system base-systems)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu services networking)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (%my-base-minimal-system
            %my-base-desktop-system))

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

    (packages (cons* (@ (gnu packages certs) nss-certs)
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
                  %base-user-accounts))))

(define %my-base-desktop-system
  (operating-system
    (inherit %my-base-minimal-system)
    
    (packages (cons* (@ (gnu packages fonts) font-iosevka)
                     (@ (gnu packages fonts) font-google-roboto)
                     (@ (gnu packages fonts) font-adobe-source-sans-pro)
                     (@ (gnu packages fonts) font-adobe-source-serif-pro)
                     (@ (gnu packages fonts) font-adobe-source-han-sans)

                     (@ (gnu packages gnome) gnome-themes-standard)
                     (@ (gnu packages gnome) gnome-themes-extra)
                     (@ (gnu packages gnome) hicolor-icon-theme)
                     (@ (gnu packages gnome) adwaita-icon-theme)
                     (@ (rsauex packages the-dot) the-dot-cursor-theme)

                     (@ (rsauex packages xorg) my-xorg)
                     (@ (gnu packages xorg) xrdb)
                     (@ (gnu packages xorg) xset)
                     (@ (gnu packages xorg) xinput)
                     (@ (gnu packages xorg) setxkbmap)

                     (@ (gnu packages m4) m4)
                     (@ (gnu packages wm) i3-wm)
                     (@ (gnu packages wm) i3status)
                     (@ (gnu packages wm) i3blocks)

                     (@ (gnu packages gnome) dconf)
                     (@ (gnu packages gnome) dconf-editor)
                     (@ (gnu packages gnome) gsettings-desktop-schemas)
                     (list (@ (gnu packages glib) glib) "bin")

                     (@ (gnu packages xdisorg) rofi)
                     (@ (gnu packages xdisorg) xss-lock)

                     (@ (gnu packages terminals) alacritty)
                     (@ (gnu packages gnuzilla) icecat)
                     (@ (gnu packages gnome) evince)
                     (@ (gnu packages password-utils) keepassxc)
                     (@ (gnu packages syncthing) syncthing-gtk)
                     (@ (gnu packages pulseaudio) pavucontrol)
                     (@ (gnu packages wine) wine)

                     (operating-system-packages %my-base-minimal-system)))

    (services (cons*
               (screen-locker-service (@ (gnu packages wm) i3lock))

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

                     (network-manager-service-type
                      config =>
                      (network-manager-configuration
                       (inherit config)
                       (dns "dnsmasq")))

                     (ntp-service-type
                      config =>
                      (ntp-configuration
                       (inherit config)
                       (allow-large-adjustment? #t)))))
                 
                 (cut remove (compose (cut eq? gdm-service-type <>) service-kind) <>)
                 (cut remove (compose (cut eq? screen-locker-service <>) service-kind) <>))

                %desktop-services)))))
