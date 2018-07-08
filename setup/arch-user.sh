#!/bin/sh

USERNAME=daniel

# Install expected tools
pacman -S git openssh --noconfirm
systemctl enable sshd.service

# Create a new user
useradd "$USERNAME"
