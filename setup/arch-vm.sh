#!/bin/sh

EFI_DIR=/sys/firmware/efi/efivars
DISK=/dev/sda
DISK1="$DISK"1
DISK2="$DISK"2

if [ -z "$(ls -A $EFI_DIR)" ]; then
  echo "Is this on EFI? ($EFI_DIR doesn't exist)"
  read -n1 -r -p "Hit any key to continue anyway..."
else
  echo "EFI system ($EFI_DIR exists!)"
fi

## Set up GPT
#############

echo "Writing new GPT..."
# Clear out the existing GPT
sgdisk -og $DISK

# EFI partition
sgdisk -n 1::+100MiB -c 1:"EFI System Partition" -t 1:ef00 $DISK

# Main partition
sgdisk -N 2 -c 2:"Root Linux Partition" -t 2:8305 $DISK

echo "Finished writing new GPT."

## Format partitions
####################

echo "Formatting partitions..."

# FAT for the EFI partition; ext4 for the main partition
mkfs.fat $DISK1
mkfs.ext4 $DISK2

