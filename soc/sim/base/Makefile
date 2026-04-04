SRCS := main.S
APP  := main

CCFLAGS = -march=rv32i -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles

LDSCRIPT = ../common/memmap.ld

TMP_PREFIX   ?= tmp/
CROSS_PREFIX ?= riscv32-unknown-elf-

.DEFAULT_GOAL := build

bin: $(TMP_PREFIX)$(APP).bin

clean:
	rm -rf $(TMP_PREFIX)

build: 
	$(MAKE)  bin

$(TMP_PREFIX)$(APP).bin: $(TMP_PREFIX)$(APP).elf
	$(CROSS_PREFIX)objcopy -O binary $^ $@
	$(CROSS_PREFIX)objdump -h $^ > $(TMP_PREFIX)$(APP).dis
	$(CROSS_PREFIX)objdump -d $^ >> $(TMP_PREFIX)$(APP).dis

$(TMP_PREFIX)$(APP).elf: $(SRCS) $(wildcard %.h)
	mkdir -p $(TMP_PREFIX)
	$(CROSS_PREFIX)gcc $(CCFLAGS) $(SRCS) -T $(LDSCRIPT) $(addprefix -I,$(INCDIR)) -o $@