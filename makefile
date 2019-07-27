all:
	stow init
	stow bash
	stow compton
	stow i3
	stow -t ~/.bin/ bin
	stow -t ~/.config/dunst/ dunst
	stow fontconfig
	stow urxvt
	stow -t ~/.config/rofi/ rofi
