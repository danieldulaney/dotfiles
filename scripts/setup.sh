#! /usr/bin/env sh

# Make sure the repo was cloned
if [ ! -d ~/.dotfiles ] ; then
    git clone --recursive https://github.com/danieldulaney/dotfiles.git ~/.dotfiles
fi

# Make sure the repo was cloned recursively
(cd ~/.dotfiles && git submodule update --init --recursive)

# Symlink to dotfiles
ln -s ~/.dotfiles/.zshrc ~
ln -s ~/.dotfiles/.tmux.conf ~
ln -s ~/.dotfiles/.vimrc ~
ln -s ~/.dotfiles/.gitconfig ~

