OUTPUT_ARCH(riscv)
ENTRY(_start)

SECTIONS
{
  . = 0x80000000;

  .bootpad : {
    . = . + 0x40;
  }

  .text : {
    *(.text.startup)
    *(.text .text.*)
  }

  .rodata : {
    *(.rodata .rodata.*)
  }

  .data : {
    *(.data .data.*)
  }

  .sdata : {
    *(.sdata .sdata.*)
  }

  .bss : {
    *(.sbss .sbss.*)
    *(.bss .bss.*)
    *(COMMON)
  }

  . = ALIGN(16);
  PROVIDE(_end = .);
  PROVIDE(end = .);

  PROVIDE(__global_pointer$ = 0x80000800);
  PROVIDE(__stack_top = 0x80100000);
}
