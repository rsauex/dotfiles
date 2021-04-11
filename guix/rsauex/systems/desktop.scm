(define-module (rsauex systems desktop)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu services security-token)
  #:use-module (gnu services networking)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu system pam)
  #:use-module (rsauex systems base)
  #:use-module (rsauex systems minimal)
  #:use-module (rsauex services login)
  #:use-module (rsauex services pam-u2f)
  #:use-module (rsauex services yubikey-session)
  #:use-module (rsauex services screen-locker)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (%my-base-desktop-system))

(define (my-network-manager-service-type services)
  (modify-services services
    (network-manager-service-type
     config =>
     (network-manager-configuration
      (inherit config)
      (dns "dnsmasq")))))

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
                     (@ (gnu packages gnome) gvfs)

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
                     (@ (rsauex packages gigolo) gigolo)
                     (@ (gnu packages libreoffice) libreoffice)

                     (@ (gnu packages security-token) yubikey-personalization)

                     (@ (rsauex packages websigner) aval-websigner)

                     (operating-system-packages %my-base-minimal-system)))

    (services (append
               (list (service my-screen-locker-service-type
                              (my-screen-locker
                               (program
                                (file-append (@ (gnu packages wm) i3lock)
                                             "/bin/i3lock"))
                               (pam-service
                                (my-user-auth-pam-service "i3lock")))))
               (list (service yubikey-session-service-type))

               %my-base-services

               ((compose
                 my-console-font-service-type
                 my-network-manager-service-type

                 (cut remove (compose (cut eq? gdm-service-type <>) service-kind) <>)
                 (cut remove (compose (cut eq? screen-locker-service-type <>) service-kind) <>)
                 (cut remove (compose (cut eq? ntp-service-type <>) service-kind) <>)
                 (cut remove (compose (cut eq? login-service-type <>) service-kind) <>))

                %desktop-services)))

    (pam-services %my-base-pam-services)))
