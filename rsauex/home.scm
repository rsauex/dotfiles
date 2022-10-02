(use-modules
 ((gnu home services fontutils) #:prefix fontutils:)
 ((gnu home services shells)    #:prefix shells:)
 ((gnu home services xdg)       #:prefix xdg:)
 ((gnu home services))
 ((gnu home))
 ((gnu packages dunst)              #:prefix dunst:)
 ((gnu packages fcitx5)             #:prefix fcitx5:)
 ((gnu packages gnome)              #:prefix gnome:)
 ((gnu packages linux)              #:prefix linux:)
 ((gnu packages m4)                 #:prefix m4:)
 ((gnu packages networking)         #:prefix networking:)
 ((gnu packages password-utils)     #:prefix passwd-utils:)
 ((gnu packages perl)               #:prefix perl:)
 ((gnu packages polkit)             #:prefix polkit:)
 ((gnu packages syncthing)          #:prefix syncthing:)
 ((gnu packages text-editors)       #:prefix text-editors:)
 ((gnu packages wm)                 #:prefix wm:)
 ((gnu packages xdisorg)            #:prefix xdisorg:)
 ((gnu packages xorg)               #:prefix xorg:)
 ((gnu services))
 ((guix modules))
 ((guix gexp))
 ((guix git))
 ((ice-9 match))
 ((ice-9 textual-ports))
 ((rsauex home services cursor-theme)   #:prefix my-cursor-theme:)
 ((rsauex home services git)            #:prefix my-git:)
 ((rsauex home services gui-startup)    #:prefix my-gui-startup:)
 ((rsauex home services rofi)           #:prefix my-rofi:)
 ((rsauex home services shepherd)       #:prefix my-shepherd:)
 ((rsauex home services ssh)            #:prefix my-ssh-service:)
 ((rsauex packages nordic-theme)        #:prefix nordic-theme:)
 ((rsauex packages the-dot)             #:prefix the-dot:)
 ((rsauex packages))
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
           ;; TODO: Remove
           ;; For scripts in i3blocks
           linux:acpi
           linux:sysstat
           perl:perl
           xorg:xset
           xdisorg:xdotool
           ;; Called from config
           xdisorg:maim
           xdisorg:xclip
           music:playerctl))
    (home-xdg-configuration-files-service-type
     `(("i3/i3blocks.conf"
        ,(rsauex-home-file "i3blocks.conf" "i3blocks.conf"))
       ("i3/i3blocks"
        ,(rsauex-home-file "i3blocks" "i3blocks-libexec" #:recursive? #t))
       ("i3/config"
        ,(computed-file
          "i3-config"
          (let ((files (list (rsauex-home-file "i3/00_base.conf" "i3-00_base.conf")
                             (rsauex-home-file "i3/05_colors.conf" "i3-05_colors.conf")
                             (rsauex-home-file "i3/10_keys.conf" "i3-10_keys.conf")
                             (rsauex-home-file "i3/15_keys.wm.conf" "i3-15_keys.wm.conf")
                             (rsauex-home-file "i3/20_menus.conf" "i3-20_menus.conf")
                             (rsauex-home-file "i3/30_bar.conf" "i3-30_bar.conf")
                             (rsauex-home-file "i3/40_client.conf" "i3-40_client.conf")))
                (m4 (file-append m4:m4 "/bin/m4"))
                (cassis-type (call-with-input-file "/sys/class/dmi/id/chassis_type"
                               (lambda (port)
                                 (get-line port)))))
            #~(begin
                (use-modules (ice-9 popen)
                             (ice-9 textual-ports))
                (with-fluids ((%default-port-encoding "UTF-8"))
                  (call-with-output-file #$output
                    (lambda (port)
                      (put-string port (get-string-all (open-pipe* OPEN_READ #$m4 (string-append "-DPC_TYPE=" #$cassis-type) #$@files))))))))))))))

(home-environment
 (packages (list text-editors:texmacs))
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
                        (list
                         ;; Better QT
                         (cons "QT_QPA_PLATFORMTHEME" "kvantum")
                         (cons "QT_STYLE_OVERRIDE" "kvantum")
                         ;; Local bin
                         (cons "PATH" "$HOME/.bin:$HOME/dotfiles/home-files/bin:$HOME/.local/bin:$PATH")
                         ;; Temp fix for Evolution to see installed plugins
                         ;; TODO: Replace with a service or a wrapper
                         (cons "EDS_EXTRA_PREFIXES" "/run/current-system/profile/")
                         ;; Fix scaling issues in Alacritty
                         (cons "WINIT_X11_SCALE_FACTOR" "1")
                         ;; Respect immodule cache (TODO: This shouldn't be necessary...)
                         (cons "GUIX_GTK2_IM_MODULE_FILE" "/run/current-system/profile/lib/gtk-2.0/2.10.0/immodules-gtk2.cache")
                         (cons "GUIX_GTK3_IM_MODULE_FILE" "/run/current-system/profile/lib/gtk-3.0/3.0.0/immodules-gtk3.cache")
                         ;; Respect QT Plugins (TODO: This shouldn't be necessary...)
                         (cons "QT_PLUGIN_PATH" "/run/current-system/profile/lib/qt5/plugins")))
        ;; GUI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (service my-gui-startup:gui-startup-service-type
                 (my-gui-startup:gui-startup-configuration
                  (program (file-append wm:i3-wm "/bin/i3"))))
        (i3-config-service)
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
          (my-gui-startup:gui-startup-service-type
           (my-gui-startup:gui-startup-extension
            (services
             (list (my-shepherd:simple-forkexec-shepherd-service
                    'blueman-applet
                    "Run `blueman-applet'"
                    #~`(#$(file-append networking:blueman "/bin/blueman-applet"))))))))
        (anon-service syncthing-applet-autostart
          (my-gui-startup:gui-startup-service-type
           (my-gui-startup:gui-startup-extension
            (services
             (list (my-shepherd:simple-forkexec-shepherd-service
                    'syncthing-applet
                    "Run `syncthing-gtk'"
                    #~`(#$(file-append syncthing:syncthing-gtk "/bin/syncthing-gtk"))))))))
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
        (anon-service dunst-autostart
          (my-gui-startup:gui-startup-service-type
           (my-gui-startup:gui-startup-extension
            (services
             (list (my-shepherd:simple-forkexec-shepherd-service
                    'dunst
                    "Run `dunst'"
                    #~`(#$(file-append dunst:dunst "/bin/dunst")))))))
          (home-xdg-configuration-files-service-type
           `(("dunst/dunstrc"
              ,(rsauex-home-file "dunstrc" "dunstrc")))))
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
                           ,(rsauex-home-file "uk-translit.mim" "uk-translit.mim")))))))
