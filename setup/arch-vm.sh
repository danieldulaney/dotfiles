#!/bin/sh

EFI_DIR=/sys/firmware/efi/efivars
EFI_PART_NUM=1
ROOT_PART_NUM=2
DISK=/dev/sda
EFI_PART="$DISK""$EFI_PART_NUM"
ROOT_PART="$DISK""$ROOT_PART_NUM"

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
sgdisk -n "$EFI_PART_NUM::+100MiB" -c "$EFI_PART_NUM:EFI System Partition" -t "$EFI_PART_NUM:ef00" $DISK

# Main partition
sgdisk -N "$ROOT_PART_NUM" -c "$ROOT_PART_NUM:Root Linux Partition" -t "$ROOT_PART_NUM:8305" $DISK

echo "Finished writing new GPT."

## Format partitions
####################

echo "Formatting partitions..."

# FAT for the EFI partition; ext4 for the main partition
yes | mkfs.fat $EFI_PART
yes | mkfs.ext4 $ROOT_PART

## Mount disks
##############

mount $ROOT_PART /mnt
mkdir /mnt/boot
mount $EFI_PART /mnt/boot

## Install base system
######################

pacstrap /mnt base
