help:
	@echo "Targets:"
	@echo " - Common:"
	@echo "    bash             - Bash stuff"
	@echo "    powershell       - Powershell stuff"
	@echo "    bin              - some utilites"
	@echo "    fontconfig       - Fonts settings"
	@echo "    git              - Git settings"
	@echo " - X specific:"
	@echo "    x-environment    - all X stuff together"
	@echo "    x-init           - automatic X and i3 startup"
	@echo "    compton          - Compton settings"
	@echo "    dunst            - Dunst notifications"
	@echo "    i3               - Tiling window manager"
	@echo "    zathura          - PDF viewer"
	@echo " - Sway specific":
	@echo "    sway-environment - all Sway stuff together"
	@echo "    sway-init        - automatic Sway startup"
	@echo "    mako             - Mako notifications"
	@echo "    sway             - Tiling window manager"
	@echo "    termite          - Termite terminal emulator"
	@echo " - Environment independent:"
	@echo "    gtk              - GTK2 and GTK3 settings"
	@echo "    xresources       - Xresources (rxvt, emacs, x3270, rofi, etc.)"
	@echo "    rofi             - Pop-up menus"
.PHONY: help

## X specific       ######

x-environment:       x-init    compton    dunst    xresources    gtk    i3    rofi    zathura
un-x-environment: un-x-init un-compton un-dunst un-xresources un-gtk un-i3 un-rofi un-zathura
.PHONY: x-environment un-x-environment

x-init:
	stow    init-X
un-x-init:
	stow -D init-X
.PHONY: x-init un-x-init

compton:
	stow    compton
	systemctl --user enable compton.service
un-compton:
	systemctl --user disable compton.service
	stow -D compton
.PHONY: compton un-compton

dunst:
	stow    dunst
un-dunst:
	stow -D dunst
.PHONY: dunst un-dunst

i3:
	stow    i3
un-i3:
	stow -D i3
.PHONY: i3 un-i3


zathura:
	stow    zathura
un-zathura:
	stow -D zathura
.PHONY: zathura un-zathura

### Sway specific   ######

sway-environment:       sway-init    mako    rofi    sway    termite
un-sway-environment: un-sway-init un-mako un-rofi un-sway un-termite
.PHONY: sway-environment un-sway-environment

sway-init:
	stow    init-Sway
un-sway-init:
	stow -D init-Sway
.PHONY: sway-init un-sway-init

sway:
	stow    sway
un-sway:
	stow -D sway
.PHONY: sway un-sway

mako:
	stow    mako
un-mako:
	stow -D mako
.PHONY: mako un-mako

termite:
	stow    termite
un-termite:
	stow -D termite
.PHONY: termite un-termite

### Env independent ######

gtk:
	stow    gtk
un-gtk:
	stow -D gtk
.PHONY: gtk un-gtk

xresources:
	stow    X
un-xresources:
	stow -D X
.PHONY: xresources un-xresources

rofi:
	stow    rofi
un-rofi:
	stow -D rofi
.PHONY: rofi un-rofi

### Common          ######

bash:
	stow    bash
un-bash:
	stow -D bash
.PHONY: bash un-bash

powershell:
	stow    powershell
un-powershell:
	stow -D powershell
.PHONY: powershell un-powershell

bin:
	stow    -t ~/.bin/ bin
un-bin:
	stow -D -t ~/.bin/ bin
.PHONY: bin un-bin

fontconfig:
	stow    fontconfig
un-fontconfig:
	stow -D fontconfig
.PHONY: fontconfig un-fontconfig

git:
	stow    git
un-git:
	stow -D git
.PHONY: git un-git 

syncthing:
	stow    syncthing
	systemctl --user enable syncthing.service
un-syncthing:
	systemctl --user disable syncthing.service
	stow -D syncthing
.PHONY: syncthing un-syncthing

nm-applet:
	stow    nm-applet
	systemctl --user enable nm-applet.service
un-nm-applet:
	systemctl --user disable nm-applet.service
	stow -D nm-applet
.PHONY: nm-applet un-nm-applet
