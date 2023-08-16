#!/usr/bin/env bash

OUTPUT="NootBoot"

KERNEL="kernel"
KERNEL_SIZE=$(stat -c %s $KERNEL)
KERNEL_PAD=$((512 - ($KERNEL_SIZE % 512)))  # padding needed for sector alignement

INITRD="initrd"
INITRD_SIZE=$(stat -c %s $INITRD)
INITRD_PAD=$((512 - ($INITRD_SIZE % 512)))  # padding needed for sector alignement

nasm nootboot.asm -o $OUTPUT

# concatenate kernel and fill up to whole sector
cat "$KERNEL" >> "$OUTPUT"
dd if=/dev/zero bs=1 count=$KERNEL_PAD >> "$OUTPUT"

# concatenate intird and fill up to whole sector
cat "$INITRD" >> "$OUTPUT"
dd if=/dev/zero bs=1 count=$INITRD_PAD >> "$OUTPUT"
