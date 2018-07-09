#!/bin/sh

USERNAME=daniel

# Install expected tools
pacman -S git openssh sudo --noconfirm
systemctl enable sshd.service
systemctl start sshd.service

# Create a new user
useradd -m "$USERNAME"
