#! /usr/bin/env bash

# Files that should be symlinked from the home directory
to_link=(.bash_profile .gitconfig .tmux.conf .vimrc)

# Directory containing this source file
dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

for file in "${to_link[@]}" ; do

    # If a symlink exists, just delete it
    if [ -h "$HOME/$file" ] ; then
        rm -f "$HOME/$file"

    # If any other file exists, rename it to .old
    elif [ -e "$HOME/$file" ] ; then
        mv "$HOME/$file" "$HOME/$file.old" 2> /dev/null

    fi

    # Create a new symlink
    ln -s "$dir/$file" "$HOME/$file"
done

