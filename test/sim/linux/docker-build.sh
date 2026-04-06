#!/bin/bash
# ─────────────────────────────────────────────────────────────────────────
# docker-build.sh — Build Linux kernel + minimal initramfs for RV1 sim
# Runs inside the Docker container.
# Output: /out/Image (ready for tb_rv --bin Image --noshift)
# ─────────────────────────────────────────────────────────────────────────
set -e

CROSS=riscv64-linux-gnu-
ARCH=riscv
JOBS=$(nproc)
OUT=/out

mkdir -p $OUT

echo "========================================"
echo " Step 1: Build minimal init (no libc)"
echo "========================================"
# Create a minimal init that uses raw syscalls — no libc needed.
# This is enough to prove the kernel boots and prints to console.
cat > /tmp/init.S << 'INITEOF'
/* Minimal /init for rv32 nommu Linux — raw syscalls + direct UART output */
/* Since the 8250 driver has no TX interrupt in sim, we also write directly
   to the UART for reliable console output. */
.section .text
.globl _start
_start:
    /* Write banner directly to UART THR at 0xC0000200 (reg-shift=2) */
    la   a1, banner
    la   a2, banner_end
    li   a3, 0xC0000200   /* UART base (THR at offset 0) */
    li   a4, 0xC0000214   /* UART LSR at offset 5*4=0x14 */
.Lwrite_loop:
    bgeu a1, a2, .Lwrite_done
    lbu  t0, 0(a1)
.Lwait_thre:
    lw   t1, 0(a4)        /* Read LSR */
    andi t1, t1, 0x20     /* Check THRE (bit 5) */
    beqz t1, .Lwait_thre
    sw   t0, 0(a3)        /* Write char to THR */
    addi a1, a1, 1
    j    .Lwrite_loop
.Lwrite_done:

    /* Write ok_msg directly too */
    la   a1, ok_msg
    la   a2, ok_end
.Lwrite_loop2:
    bgeu a1, a2, .Lwrite_done2
    lbu  t0, 0(a1)
.Lwait_thre2:
    lw   t1, 0(a4)
    andi t1, t1, 0x20
    beqz t1, .Lwait_thre2
    sw   t0, 0(a3)
    addi a1, a1, 1
    j    .Lwrite_loop2
.Lwrite_done2:

    /* exit(0)  —  syscall 93 */
    li   a7, 93
    li   a0, 0
    ecall

    /* should not reach here */
1:  j    1b

.section .rodata
banner:
    .ascii "\n"
    .ascii "=====================================\n"
    .ascii " RV1 nommu Linux — boot successful!\n"
    .ascii "=====================================\n"
    .ascii "\n"
banner_end:

ok_msg:
    .ascii "[init] Hello from userspace on RV1!\n"
    .ascii "[init] System halting.\n"
ok_end:
INITEOF

${CROSS}as -march=rv32imac -mabi=ilp32 -o /tmp/init.o /tmp/init.S
${CROSS}ld -m elf32lriscv -pie --no-dynamic-linker -o /tmp/init /tmp/init.o
${CROSS}strip /tmp/init
echo "init: $(stat -c%s /tmp/init) bytes (ELF type: $(${CROSS}readelf -h /tmp/init | grep Type | awk '{print $2}'))"

echo "========================================"
echo " Step 2: Build initramfs cpio"
echo "========================================"
INITRAMFS=/build/initramfs_root
rm -rf $INITRAMFS
mkdir -p $INITRAMFS/{dev,proc,sys,tmp}

cp /tmp/init $INITRAMFS/init
chmod +x $INITRAMFS/init

# Build cpio using a file list (avoids mknod permission issues on bind mounts).
# The kernel's initramfs build infrastructure can consume a file list directly.
cat > /build/initramfs_list << 'CPIOLIST'
dir /dev 0755 0 0
dir /proc 0755 0 0
dir /sys 0755 0 0
dir /tmp 0755 0 0
nod /dev/console 0600 0 0 c 5 1
CPIOLIST
echo "file /init $INITRAMFS/init 0755 0 0" >> /build/initramfs_list
echo "initramfs_list: ready (with /dev/console device node)"

echo "========================================"
echo " Step 3: Install DTS + configure kernel"
echo "========================================"
cd /build/linux

# ── Install our device tree into the kernel source tree ──
# Create a vendor directory for the RV1 DTS
mkdir -p arch/riscv/boot/dts/rv1
cp /build/rv1.dts arch/riscv/boot/dts/rv1/rv1.dts

# Create the vendor Makefile so the DTB gets compiled and linked in
cat > arch/riscv/boot/dts/rv1/Makefile << 'DTSMK'
dtb-y += rv1.dtb
obj-$(CONFIG_BUILTIN_DTB) += rv1.dtb.o
DTSMK

# Register the rv1 sub-directory in the parent Makefile
if ! grep -q 'subdir-y += rv1' arch/riscv/boot/dts/Makefile; then
    echo 'subdir-y += rv1' >> arch/riscv/boot/dts/Makefile
fi

# Start from the rv32 nommu defconfig
make ARCH=$ARCH CROSS_COMPILE=$CROSS rv32_nommu_virt_defconfig

# Merge our fragment (includes CONFIG_BUILTIN_DTB=y)
scripts/kconfig/merge_config.sh -m .config /build/rv1_defconfig_fragment

# Point initramfs to our cpio
sed -i 's|CONFIG_INITRAMFS_SOURCE=""|CONFIG_INITRAMFS_SOURCE="/build/initramfs_list"|' .config

# Ensure the CLINT timer driver and 8250 UART are on
scripts/config --enable CONFIG_RISCV_TIMER
scripts/config --enable CONFIG_SERIAL_8250
scripts/config --enable CONFIG_SERIAL_8250_CONSOLE
scripts/config --enable CONFIG_SERIAL_EARLYCON
scripts/config --enable CONFIG_BUILTIN_DTB
scripts/config --disable CONFIG_RISCV_SBI

# Make sure config is consistent
make ARCH=$ARCH CROSS_COMPILE=$CROSS olddefconfig

echo "========================================"
echo " Step 4: Build Linux kernel"
echo "========================================"
make ARCH=$ARCH CROSS_COMPILE=$CROSS -j$JOBS 2>&1 | tail -30

# The output is arch/riscv/boot/Image (flat binary)
if [ ! -f arch/riscv/boot/Image ]; then
    echo "ERROR: Kernel Image not found!"
    echo "Checking for build errors..."
    make ARCH=$ARCH CROSS_COMPILE=$CROSS -j1 2>&1 | tail -50
    exit 1
fi

cp arch/riscv/boot/Image $OUT/Image
cp System.map $OUT/System.map
echo "Kernel Image: $(du -h $OUT/Image | cut -f1)"

echo "========================================"
echo " Step 5: Build DTB and composite binary"
echo "========================================"
dtc -I dts -O dtb -o $OUT/rv1.dtb /build/rv1.dts
echo "DTB: $(du -h $OUT/rv1.dtb | cut -f1)"

# Assemble composite: Image + DTB (placed AFTER kernel BSS end)
# The BSS extends beyond the Image file.  We must place the DTB
# past __bss_stop so the kernel's BSS-clearing loop doesn't erase it.
BSS_STOP=$(${CROSS}nm vmlinux 2>/dev/null | awk '/__bss_stop/{print "0x"$1}')
if [ -z "$BSS_STOP" ]; then
    echo "WARNING: __bss_stop not found in vmlinux, using Image size + 512K"
    KERNEL_SIZE=$(stat -c%s $OUT/Image)
    DTB_OFFSET=$(( (KERNEL_SIZE + 0x80000 + 0xFFF) & ~0xFFF ))
else
    # BSS_STOP is a virtual address (0x80xxxxxx); convert to file offset
    BSS_STOP_DEC=$(printf "%d" $BSS_STOP)
    DTB_OFFSET=$(( (BSS_STOP_DEC - 0x80000000 + 0xFFF) & ~0xFFF ))
fi
DTB_ADDR=$(( 0x80000000 + DTB_OFFSET ))
DTB_ADDR_HEX=$(printf "0x%08x" $DTB_ADDR)
DTB_SIZE=$(stat -c%s $OUT/rv1.dtb)
TOTAL_SIZE=$(( DTB_OFFSET + DTB_SIZE ))

echo "BSS end:     ${BSS_STOP:-unknown}"
echo "DTB offset:  $(printf '0x%x' $DTB_OFFSET)"
echo "DTB addr:    $DTB_ADDR_HEX"

cp $OUT/Image $OUT/linux.bin
truncate -s $DTB_OFFSET $OUT/linux.bin
dd if=$OUT/rv1.dtb of=$OUT/linux.bin bs=4096 seek=$((DTB_OFFSET / 4096)) conv=notrunc 2>/dev/null

# The kernel Image has a 64-byte RISC-V header:
#   Image[0x00] = code0 (jump instruction to offset 0x40)
#   Image[0x40] = _start_kernel (real entry point)
# With --noshift, the testbench loads at 0x80000000.
# Boot PC is 0x80000040 = Image offset 0x40 = kernel entry.
# a0=0 (hartid, regs init to 0), a1 set by --dtb-addr.

echo ""
echo "========================================"
echo " Build complete!"
echo "========================================"
echo "  $OUT/linux.bin  ($(du -h $OUT/linux.bin | cut -f1), tb_rv ready)"
echo "  $OUT/Image      (kernel only)"
echo "  $OUT/rv1.dtb    (device tree blob)"
echo ""
echo "DTB_ADDR=$DTB_ADDR_HEX" > $OUT/build_info.txt
echo ""
echo "Run with:"
echo "  tb_rv --bin linux.bin --noshift --dtb-addr $DTB_ADDR_HEX --cycles 500000000"
echo "========================================"
