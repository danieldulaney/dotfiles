#!/bin/sh

EFI_DIR=/sys/firmware/efi/efivars
DISK=/dev/sda

if [ -z "$(ls -A $EFI_DIR)" ]; then
  echo "Is this on EFI? ($EFI_DIR doesn't exist)"
  read -n1 -r -p "Hit any key to continue anyway..."
else
  echo "EFI system ($EFI_DIR exists!)"
fi

echo "Writing new GPT"
# Clear out the existing GPT
sgdisk -og $DISK
# Create and configure the EFI partition
sgdisk -n 2::+100MiB -c 2:"EFI System Partition" -t 2:ef00 $DISK
