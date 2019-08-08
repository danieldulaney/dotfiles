[[ -f ~/.Xresources ]] && xrdb -I$HOME ~/.Xresources
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

export LANG=en_US.UTF-8

# Disable caps lock
udiskie &
compton &
awesome
