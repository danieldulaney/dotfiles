#! /usr/bin/env sh

# Make sure the repo was cloned
if [ ! -d ~/.dotfiles ] ; then
    git clone --recursive https://github.com/danieldulaney/dotfiles.git ~/.dotfiles
fi

# Make sure the repo was cloned recursively
(cd ~/.dotfiles && git submodule update --init --recursive)

# Symlink to dotfiles
ln -fs ~/.dotfiles/.zshrc ~
ln -fs ~/.dotfiles/.tmux.conf ~
ln -fs ~/.dotfiles/.vimrc ~
ln -fs ~/.dotfiles/.gitconfig ~
ln -fs ~/.dotfiles/.xmonad ~
ln -fs ~/.dotfiles/.Xresources ~
ln -fs ~/.dotfiles/.Xresources.d ~
ln -fs ~/.dotfiles/.fehbg ~
ln -fs ~/.dotfiles/.ghci ~
