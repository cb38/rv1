APP        := float_printf
MARCH      := rv32imc_zicsr_zbs
CROSS      := riscv32-unknown-elf-
CC         := $(CROSS)gcc
OBJCOPY    := $(CROSS)objcopy
OBJDUMP    := $(CROSS)objdump

CFLAGS     := -march=$(MARCH) -mabi=ilp32 -O2 -g \
              -ffreestanding -nostdlib -nostartfiles \
              -Wl,--no-warn-rwx-segments \
              -I../common -I../tb_common/include \
              -I../coremark/dist -I../coremark/dist/barebones

LDSCRIPT   := ../common/memmap.ld
LFLAGS     := -T $(LDSCRIPT) -lm -lc -lnosys -lgcc

EE_PRINTF  := ../coremark/dist/barebones/ee_printf.c
CVT        := ../coremark/dist/barebones/cvt.c
SRCS       := ../common/init_rv.S main.c $(EE_PRINTF) $(CVT)

TBEXEC     := ../tb_cxxrtl/tb_rv
MAX_CYCLES ?= 50000000

TMP        := tmp/

.PHONY: all run clean

all: run

run: $(TMP)$(APP).bin
	$(TBEXEC) --bin $< --cycles $(MAX_CYCLES)

$(TMP)$(APP).bin: $(TMP)$(APP).elf
	$(OBJCOPY) -O binary $< $@
	$(OBJDUMP) -h $< > $(TMP)$(APP).dis
	$(OBJDUMP) -d $< >> $(TMP)$(APP).dis

$(TMP)$(APP).elf: $(SRCS) | $(TMP)
	$(CC) $(CFLAGS) $(SRCS) $(LFLAGS) -o $@

$(TMP):
	mkdir -p $(TMP)

clean:
	rm -rf $(TMP)
