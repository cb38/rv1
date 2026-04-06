OUTPUT_ARCH(riscv)
ENTRY(_start)

SECTIONS
{
    . = 0x80000000;

    /* 64-byte boot pad (hardware reset vector area) */
    .bootpad : {
        . = . + 0x40;
    }

    /* Loader code starts at 0x80000040 */
    .text : {
        *(.text.init)
        *(.text .text.*)
    }

    . = ALIGN(4);

    /* Kernel Image is loaded at fixed offset 0x800 from RAM base */
    . = 0x80000800;
    .kernel : {
        _kernel_start = .;
        KEEP(*(.kernel))
        _kernel_end = .;
    }

    . = ALIGN(4);

    /* DTB follows kernel */
    .dtb : {
        _dtb_start = .;
        KEEP(*(.dtb))
        _dtb_end = .;
    }

    . = ALIGN(4);

    /* initramfs (if appended) */
    .initramfs : {
        _initramfs_start = .;
        KEEP(*(.initramfs))
        _initramfs_end = .;
    }

    . = ALIGN(16);
    _end = .;

    PROVIDE(__stack_top = 0x80F00000);
}
