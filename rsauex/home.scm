(use-modules
 ((gnu home services fontutils) #:prefix fontutils:)
 ((gnu home services shells)    #:prefix shells:)
 ((gnu home services xdg)       #:prefix xdg:)
 ((gnu home services))
 ((gnu home))
 ((gnu packages admin)              #:prefix admin:)
 ((gnu packages base)               #:prefix base-packages:)
 ((gnu packages compression)        #:prefix compression:)
 ((gnu packages docker)             #:prefix docker:)
 ((gnu packages dunst)              #:prefix dunst:)
 ((gnu packages emacs)              #:prefix emacs:)
 ((gnu packages fcitx5)             #:prefix fcitx5:)
 ((gnu packages fonts)              #:prefix fonts:)
 ((gnu packages gimp)               #:prefix gimp:)
 ((gnu packages gnome)              #:prefix gnome:)
 ((gnu packages gnome-xyz)          #:prefix gnome-xyz:)
 ((gnu packages gtk)                #:prefix gtk:)
 ((gnu packages image-viewers)      #:prefix image-viewers:)
 ((gnu packages kde-frameworks)     #:prefix kde-frameworks:)
 ((gnu packages libreoffice)        #:prefix libreoffice:)
 ((gnu packages linux)              #:prefix linux:)
 ((gnu packages music)              #:prefix music:)
 ((gnu packages networking)         #:prefix networking:)
 ((gnu packages password-utils)     #:prefix passwd-utils:)
 ((gnu packages pdf)                #:prefix pdf:)
 ((gnu packages perl)               #:prefix perl:)
 ((gnu packages polkit)             #:prefix polkit:)
 ((gnu packages pulseaudio)         #:prefix pulseaudio:)
 ((gnu packages qt)                 #:prefix qt:)
 ((gnu packages rsync)              #:prefix rsync:)
 ((gnu packages scanner)            #:prefix scanner:)
 ((gnu packages security-token)     #:prefix security-token:)
 ((gnu packages ssh)                #:prefix ssh:)
 ((gnu packages syncthing)          #:prefix syncthing:)
 ((gnu packages terminals)          #:prefix terms:)
 ((gnu packages text-editors)       #:prefix text-editors:)
 ((gnu packages version-control)    #:prefix vc:)
 ((gnu packages video)              #:prefix video:)
 ((gnu packages web)                #:prefix web:)
 ((gnu packages wine)               #:prefix wine:)
 ((gnu packages wm)                 #:prefix wm:)
 ((gnu packages xdisorg)            #:prefix xdisorg:)
 ((gnu packages xorg)               #:prefix xorg:)
 ((gnu services))
 ((guix gexp))
 ((guix git))
 ((guix modules))
 ((ice-9 match))
 ((ice-9 textual-ports))
 ((nongnu packages mozilla)             #:prefix mozilla:)
 ((rsauex home services cursor-theme)   #:prefix my-cursor-theme:)
 ((rsauex home services git)            #:prefix my-git:)
 ((rsauex home services gui-startup)    #:prefix my-gui-startup:)
 ((rsauex home services rofi)           #:prefix my-rofi:)
 ((rsauex home services shepherd)       #:prefix my-shepherd:)
 ((rsauex home services ssh)            #:prefix my-ssh-service:)
 ((rsauex packages fcitx5)              #:prefix my-fcitx5:)
 ((rsauex packages gigolo)              #:prefix gigolo:)
 ((rsauex packages kvantum)             #:prefix kvantum:)
 ((rsauex packages nordic-theme)        #:prefix nordic-theme:)
 ((rsauex packages powershell)          #:prefix powershell:)
 ((rsauex packages the-dot)             #:prefix the-dot:)
 ((rsauex packages gns3)                #:prefix gns3:)
 ((rsauex packages))
 ((rsauex script template))
 ((rsauex services))
 ((srfi srfi-1))
 ((srfi srfi-26)))

(define (rsauex-module-name? name)
  (match name
    (('rsauex _ ...) #t)
    (_ #f)))

(define (program-fn-file module fn)
  (program-file
   (string-join (map symbol->string (append module (list fn))) "-")
   (with-imported-modules
       (append (list module)
               (source-module-closure
                (list module)
                #:select? rsauex-module-name?))
     #~(apply (@ #$module #$fn) (cdr (program-arguments))))))

(define (git-prevent-push-to-important-branches)
  (cons #:pre-push (program-fn-file '(rsauex home services git hooks ask-on-push-to-master) 'check)))

(define (git-dont-push-wip-commits)
  (cons #:pre-push (program-fn-file '(rsauex home services git hooks dont-push-wip-commits) 'check)))

(define (my-essential-services he)
  ((compose
    (cut remove (compose (cut eq? fontutils:home-fontconfig-service-type <>) service-kind) <>))
   ((@@ (gnu home) home-environment-default-essential-services) he)))

(define (i3-config-service)
  (anon-service i3blocks-config
    (home-profile-service-type
     (list wm:i3-wm
           wm:i3status
           wm:i3blocks
           ;; For scripts in i3blocks (TODO: Remove)
           linux:acpi
           linux:sysstat
           perl:perl
           xorg:xset
           xdisorg:xclip
           music:playerctl
           ;; Called from i3 scripts (TODO: Remove)
           xdisorg:xdotool
           web:jq))
    (home-xdg-configuration-files-service-type
     `(("i3/i3blocks.conf"
        ,(rsauex-home-file "i3blocks.conf" "i3blocks.conf"))
       ("i3/i3blocks"
        ,(rsauex-home-file "i3blocks" "i3blocks-libexec" #:recursive? #t))
       ("i3/config"
        ,(computed-file
          "i3-config"
          (let ((files (list (rsauex-home-template-file "i3/00_base.conf" "i3-00_base.conf")
                             (rsauex-home-template-file "i3/05_colors.conf" "i3-05_colors.conf")
                             (rsauex-home-template-file "i3/10_keys.conf" "i3-10_keys.conf")
                             (rsauex-home-template-file "i3/15_keys.wm.conf" "i3-15_keys.wm.conf")
                             (rsauex-home-template-file "i3/20_menus.conf" "i3-20_menus.conf")
                             (rsauex-home-template-file "i3/30_bar.conf" "i3-30_bar.conf")
                             (rsauex-home-template-file "i3/40_client.conf" "i3-40_client.conf")))
                (cat (file-append base-packages:coreutils "/bin/cat")))
            #~(begin
                (use-modules (ice-9 popen)
                             (ice-9 textual-ports))
                (with-fluids ((%default-port-encoding "UTF-8"))
                  (call-with-output-file #$output
                    (lambda (port)
                      (put-string port (get-string-all (open-pipe* OPEN_READ #$cat #$@files))))))))))))))

(define (dunst-service)
  (anon-service dunst-autostart
    (home-profile-service-type
     (list dunst:dunst))
    (my-gui-startup:gui-startup-service-type
     (my-gui-startup:gui-startup-extension
      (services
       (list (my-shepherd:simple-forkexec-shepherd-service
              'dunst
              "Run `dunst'"
              #~`(#$(file-append dunst:dunst "/bin/dunst")))))))
    (home-xdg-configuration-files-service-type
     `(("dunst/dunstrc"
        ,(rsauex-home-file "dunstrc" "dunstrc"))))))

(define (syncthing-service)
  (anon-service syncthing-applet-autostart
    (my-gui-startup:gui-startup-service-type
     (my-gui-startup:gui-startup-extension
      (services
       (list (my-shepherd:simple-forkexec-shepherd-service
              'syncthing-applet
              "Run `syncthing-gtk'"
              #~`(#$(file-append syncthing:syncthing-gtk "/bin/syncthing-gtk"))
              #:data-packages (list syncthing:syncthing-gtk))))))))

(define (pipewire-service)
  (anon-service pipewire
    (home-profile-service-type
     (list linux:pipewire
           linux:wireplumber))
    (my-gui-startup:gui-startup-service-type
     (my-gui-startup:gui-startup-extension
      (services
       (list (my-shepherd:simple-forkexec-shepherd-service
              'pipewire
              "Run `pipewire'"
              #~`(#$(file-append linux:pipewire "/bin/pipewire")))
             (my-shepherd:simple-forkexec-shepherd-service
              'pipewire-pulse
              "Run `pipewire-pulse'"
              (let ((pipewire-pulse-wrapper
                     (program-file
                      "pipewire-pulse-wrapper"
                      #~(let ((pulseaudio-bin #$(file-append pulseaudio:pulseaudio "/bin"))
                              (pipewire-pulse #$(file-append linux:pipewire "/bin/pipewire-pulse")))
                          (setenv "PATH" (string-join (list (getenv "PATH") pulseaudio-bin) ":"))
                          (execl pipewire-pulse pipewire-pulse)))))
                #~`(#$pipewire-pulse-wrapper))
              #:requirement '(pipewire))
             (my-shepherd:simple-forkexec-shepherd-service
              'wireplumber
              "Run `wireplumber'"
              #~`(#$(file-append linux:wireplumber "/bin/wireplumber"))
              #:requirement '(pipewire))))))
    (home-xdg-configuration-files-service-type
     `(("pipewire/pipewire.conf.d/99-input-denoising.conf"
        ,(rsauex-home-template-file "pipewire/pipewire.conf.d/99-input-denoising.conf"
                                    "pipewire-99-input-denoising.conf"))))))

(home-environment
 (packages (list fonts:font-iosevka-term
                 fonts:font-google-roboto
                 fonts:font-google-noto
                 fonts:font-adobe-source-sans-pro
                 fonts:font-adobe-source-serif-pro
                 fonts:font-adobe-source-han-sans
                 fonts:font-awesome
                 fonts:font-google-material-design-icons

                 gnome:gnome-themes-standard
                 gnome:gnome-themes-extra
                 gnome:hicolor-icon-theme
                 gnome:adwaita-icon-theme
                 kde-frameworks:breeze-icons
                 nordic-theme:nordic-darker-theme
                 nordic-theme:nordic-darker-kvantum-theme

                 ;; TODO: make separate service
                 fcitx5:fcitx5
                 fcitx5:fcitx5-configtool
                 fcitx5:fcitx5-qt
                 (list fcitx5:fcitx5-gtk "gtk2")
                 (list fcitx5:fcitx5-gtk "gtk3")
                 my-fcitx5:fcitx5-m17n

                 ;; TODO: make separate service
                 kvantum:kvantum
                 qt:qt5ct

                 ;; TODO: should be specified only in syncthing-gtk's config
                 syncthing:syncthing

                 ;; TODO: needed for my emacs package
                 rsync:rsync

                 compression:zip
                 compression:unzip
                 compression:p7zip
                 security-token:yubikey-personalization
                 security-token:python-yubikey-manager
                 ssh:openssh
                 admin:htop
                 powershell:powershell
                 text-editors:texmacs
                 mozilla:firefox
                 terms:alacritty
                 xdisorg:arandr
                 libreoffice:libreoffice
                 scanner:xsane
                 passwd-utils:keepassxc
                 pulseaudio:pavucontrol
                 gigolo:gigolo
                 pdf:qpdfview
                 gnome:system-config-printer
                 docker:docker-compose
                 wine:wine
                 video:vlc
                 emacs:emacs
                 gimp:gimp
                 image-viewers:viewnior
                 vc:git
                 (list vc:git "gui")
                 gns3:gns3-gui))
 (essential-services
  (my-essential-services this-home-environment))
 (services
  (list (service
         shells:home-bash-service-type
         (shells:home-bash-configuration
          (bash-profile
           (list (rsauex-home-file ".bash_profile" "bash_profile")))
          (bashrc
           (list (rsauex-home-file ".bashrc" "bashrc")))
          (bash-logout
           (list (rsauex-home-file ".bash_logout" "bash_logout")))))
        (service my-rofi:rofi-service-type
                 (my-rofi:rofi-configuration
                  (config
                   '(("font" . "Monospace 12")))
                  (theme
                   (file-append nordic-theme:rofi-nord-theme
                                "/share/rofi/themes/nord.rasi"))))
        (service xdg:home-xdg-user-directories-service-type)
        (simple-service 'my-environment
                        home-environment-variables-service-type
                        (let ((qt-platform-plugin-path (string-append (getenv "HOME") "/.guix-home/profile/lib/qt5/plugins"))
                              (qt-plugin-paths #~(list #$(string-append (getenv "HOME") "/.guix-home/profile/lib/qt5/plugins")
                                                       "/run/current-system/profile/lib/qt5/plugins"
                                                       #$(file-append qt:qtsvg-5 "/lib/qt5/plugins")))
                              (gtk-engine-paths #~(list #$(file-append gtk:murrine "/lib/gtk-2.0")
                                                        #$(file-append gnome:gnome-themes-extra "/lib/gtk-2.0"))))
                          (list
                           ;; Better DE compatibility
                           (cons "XDG_CURRENT_DESKTOP" "XFCE")
                           ;; Better QT
                           (cons "QT_QPA_PLATFORMTHEME" "qt5ct")
                           ;; Local bin
                           (cons "PATH" "$HOME/.bin:$HOME/dotfiles/home-files/bin:$HOME/.local/bin:$PATH")
                           ;; Fix scaling issues in Alacritty
                           (cons "WINIT_X11_SCALE_FACTOR" "1")
                           ;; Respect immodule cache (TODO: This shouldn't be necessary...)
                           (cons "GUIX_GTK2_IM_MODULE_FILE" (string-append (getenv "HOME") "/.guix-home/profile/lib/gtk-2.0/2.10.0/immodules-gtk2.cache"))
                           (cons "GUIX_GTK3_IM_MODULE_FILE" (string-append (getenv "HOME") "/.guix-home/profile/lib/gtk-3.0/3.0.0/immodules-gtk3.cache"))
                           ;; Respect QT Plugins (TODO: This shouldn't be necessary...)
                           (cons "QT_QPA_PLATFORM_PLUGIN_PATH" qt-platform-plugin-path)
                           (cons "QT_PLUGIN_PATH" #~(string-join #$qt-plugin-paths ":"))
                           ;; GTK2 engines (TODO: this should be search-paths from nordic-theme)
                           (cons "GUIX_GTK2_PATH" #~(string-join #$gtk-engine-paths ":"))
                           ;; Enable video hardware acceleration (TODO: Send a patch to mozilla?)
                           (cons "MOZ_DISABLE_RDD_SANDBOX" "1"))))
        ;; GUI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (service my-gui-startup:gui-startup-service-type
                 (my-gui-startup:gui-startup-configuration
                  (program (file-append wm:i3-wm "/bin/i3"))))
        (i3-config-service)
        (dunst-service)
        (syncthing-service)
        (pipewire-service)
        ;; Autostart ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (service my-ssh-service:ssh-agent-service-type)
        (anon-service load-xresources
          (my-gui-startup:gui-startup-service-type
           (my-gui-startup:gui-startup-extension
            (services
             (list (my-shepherd:simple-one-shot-shepherd-service
                    'load-xresources
                    "Load XResources"
                    #~(lambda ()
                        (invoke #$(file-append xorg:xrdb "/bin/xrdb")
                                "-merge" (string-append "-I" (getenv "HOME"))
                                #$(rsauex-home-file ".Xresources" "Xresources"))
                        #t)))))))
        (anon-service xset-settings
          (my-gui-startup:gui-startup-service-type
           (my-gui-startup:gui-startup-extension
            (services
             (list (my-shepherd:simple-one-shot-shepherd-service
                    'xset-settings
                    "Set `xset' settings"
                    #~(lambda ()
                        (let ((xset (lambda args
                                      (apply invoke #$(file-append xorg:xset "/bin/xset") args))))
                          ;; Disable screen saver
                          (xset "s" "off")
                          ;; DPMS settings
                          (xset "dpms" "0" "600" "900")
                          ;; Cursor speed
                          (xset "r" "rate" "400" "44"))
                        #t)))))))
        (anon-service keyboard-settings
          (my-gui-startup:gui-startup-service-type
           (my-gui-startup:gui-startup-extension
            (services
             (list (my-shepherd:simple-one-shot-shepherd-service
                    'keyboard-settings
                    "Set keyboard settings"
                    #~(begin
                        (use-modules (guix build utils)
                                     (ice-9 popen)
                                     (ice-9 textual-ports))
                        (lambda ()
                          (let ((setxkbmap (lambda args
                                             (apply invoke #$(file-append xorg:setxkbmap "/bin/setxkbmap") args)))
                                (invoke/capture-stdout (lambda (program . args)
                                                         (get-string-all (apply open-pipe* OPEN_READ program args)))))
                            ;; Keyboard settings (default)
                            (setxkbmap "-option"
                                       "-option" "ctrl:nocaps"
                                       "-layout" "us"
                                       "-variant" "dvp")
                            ;; Keyboard settings (Ergodox EZ)
                            (let ((device (invoke/capture-stdout #$(file-append xorg:xinput "/bin/xinput")
                                                                 "list"
                                                                 "--id-only" "keyboard:ZSA Technology Labs Inc ErgoDox EZ")))
                              (unless (string-null? device)
                                (setxkbmap "-device" device
                                           "-option"
                                           "-layout" "us"))))
                          #t))))))))
        (anon-service network-manager-applet-autostart
          (my-gui-startup:gui-startup-service-type
           (my-gui-startup:gui-startup-extension
            (services
             (list (my-shepherd:simple-forkexec-shepherd-service
                    'network-manager-applet
                    "Run `nm-applet'"
                    #~`(#$(file-append gnome:network-manager-applet "/bin/nm-applet"))))))))
        (anon-service blueman-applet-autostart
          (home-profile-service-type
           (list networking:blueman))
          (my-gui-startup:gui-startup-service-type
           (my-gui-startup:gui-startup-extension
            (services
             (list (my-shepherd:simple-forkexec-shepherd-service
                    'blueman-applet
                    "Run `blueman-applet'"
                    #~`(#$(file-append networking:blueman "/bin/blueman-applet"))))))))
        (anon-service fcitx5-autostart
          (my-gui-startup:gui-startup-service-type
           (my-gui-startup:gui-startup-extension
            (services
             (list (my-shepherd:simple-forkexec-shepherd-service
                    'fcitx5
                    "Run `fcixt5'"
                    #~`(#$(file-append fcitx5:fcitx5 "/bin/fcitx5")))))
            (environment
             (list (cons "GTK_IM_MODULE" #~"fcitx")
                   (cons "QT_IM_MODULE" #~"fcitx")
                   (cons "XMODIFIERS" #~"@im=fcitx"))))))
        (anon-service polkit-authentication-agent-autostart
          (my-gui-startup:gui-startup-service-type
           (my-gui-startup:gui-startup-extension
            (services
             (list (my-shepherd:simple-forkexec-shepherd-service
                    'polkit-authentication-agent
                    "Run polkit-authenticator agent"
                    #~`(#$(file-append polkit:polkit-gnome "/libexec/polkit-gnome-authentication-agent-1"))))))))
        ;; Screen locking (TODO: locking script)
        (anon-service xss-lock-autostart
          (my-gui-startup:gui-startup-service-type
           (my-gui-startup:gui-startup-extension
            (services
             (list (my-shepherd:simple-forkexec-shepherd-service
                    'xss-lock
                    "Run `xss-lock'"
                    #~`(#$(file-append xdisorg:xss-lock "/bin/xss-lock")
                        "-l" "--" "lock")))))))
        (anon-service keepassxc-autostart
          (my-gui-startup:gui-startup-service-type
           (my-gui-startup:gui-startup-extension
            (services
             (list (my-shepherd:simple-forkexec-shepherd-service
                    'keepassxc
                    "Run `keepassxc'"
                    #~`(#$(file-append passwd-utils:keepassxc "/bin/keepassxc"))))))))
        ;; Git settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (service my-git:git-service-type
                 (my-git:git-configuration
                  (hooks (list (git-dont-push-wip-commits)
                               (git-prevent-push-to-important-branches)))))
        ;; Other settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (simple-service 'tmux
                        home-files-service-type
                        `((".tmux.conf"
                           ,(rsauex-home-file ".tmux.conf" "tmux.conf"))))
        (service my-cursor-theme:cursor-theme-service-type
                 (my-cursor-theme:cursor-theme-configuration
                  (theme-package the-dot:the-dot-cursor-theme)
                  (theme-name "Dot-Light")))
        (simple-service 'kvantum-theme
                        home-xdg-configuration-files-service-type
                        `(("Kvantum/kvantum.kvconfig"
                           ,(plain-file
                             "kvantum.kvconfig"
                             "theme=Nordic-Darker\n"))))
        (simple-service 'qt5ct-conf
                        home-xdg-configuration-files-service-type
                        `(("qt5ct/qt5ct.conf"
                           ,(rsauex-home-file "qt5ct.conf" "qt5ct.conf"))))
        (simple-service 'font-config
                        home-xdg-configuration-files-service-type
                        `(("fontconfig/fonts.conf"
                           ,(rsauex-home-file "fonts.conf" "fonts.conf"))))
        (simple-service 'alacritty
                        home-xdg-configuration-files-service-type
                        `(("alacritty/alacritty.yml"
                           ,(rsauex-home-file "alacritty.yml" "alacritty.yml"))))
        (simple-service 'gtk2
                        home-files-service-type
                        `((".gtkrc-2.0"
                           ,(rsauex-home-file ".gtkrc-2.0" "gtkrc-2.0"))))
        (simple-service 'gtk3
                        home-xdg-configuration-files-service-type
                        `(("gtk-3.0/settings.ini"
                           ,(rsauex-home-file "gtk-3.0-settings.ini" "gtk-3.0-settings.ini"))))
        (simple-service 'x-resources
                        home-files-service-type
                        `((".Xresources"
                           ,(rsauex-home-file ".Xresources" "Xresources"))
                          (".x3270pro"
                           ,(rsauex-home-file ".x3270pro" "x3270pro"))))
        (simple-service 'terminal-x-resources
                        home-xdg-configuration-files-service-type
                        `(("urxvt"
                           ,(rsauex-home-file "urxvt" "urxvt" #:recursive? #t))))
        (simple-service 'git-conf
                        home-files-service-type
                        `((".gitconfig"
                           ,(rsauex-home-file ".gitconfig" "gitconfig"))))
        (simple-service 'powershell
                        home-xdg-configuration-files-service-type
                        `(("powershell"
                           ,(rsauex-home-file "powershell" "powershell-config" #:recursive? #t))))
        (simple-service 'ukrainian-layout
                        home-files-service-type
                        `((".m17n.d/uk-translit.mim"
                           ,(rsauex-home-file "uk-translit.mim" "uk-translit.mim"))))
        (simple-service 'ssh-config
                        home-files-service-type
                        `((".ssh/config"
                           ,(rsauex-home-file "ssh-config" "ssh-config")))))))
