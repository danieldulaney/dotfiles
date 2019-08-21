#! /usr/bin/env bash

function yesno() {
    while true ; do
        read -r -p "${1:-Are You Sure?} [Y/n] " input

        case $input in
            [yY][eE][sS]|[yY])
                return 0
                ;;
            [nN][oO]|[nN])
                return 1
                ;;
            '')
                return 0
                ;;
            *)
                echo 'Yes or no'
                ;;
        esac
    done
}

cd

# Make sure the repo was cloned recursively
###########################################
echo "Git recursive cloning"
git submodule update --init --recursive

# Install required packages
###########################

echo "Checking required packages..."

pkglist=("zsh" "kitty" "awesome" "compton" "udiskie")

for pkg in ${pkglist[@]} ; do
    if ! pacman -Q $pkg; then
        sudo pacman -S $pkg
    fi
done

# Enable systemd user units
###########################

unitlist=("compton.service" "udiskie.service")

for unit in ${unitlist[@]} ; do
    if yesno "Start $unit?"; then
        systemctl --user start "$unit"
    fi

    if yesno "Enable $unit?"; then
        systemctl --user enable "$unit"
    fi
done
