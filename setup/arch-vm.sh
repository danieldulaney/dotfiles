#!/bin/sh

EFI_DIR=/sys/firmware/efi/efivars

if [ -z "$(ls -A $EFI_DIR)" ]; then
  echo "Is this on EFI? ($EFI_DIR doesn't exist)"
  exit 1
else
  echo "EFI system ($EFI_DIR exists!)"
fi
