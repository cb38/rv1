SRCS := main.S
APP  := atomics

CCFLAGS = -march=rv32ia -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles

LDSCRIPT = ../common/memmap.ld

TMP_PREFIX   ?= tmp/
CROSS_PREFIX ?= riscv32-unknown-elf-

.DEFAULT_GOAL := bin
.PHONY: bin clean

bin: $(TMP_PREFIX)$(APP).bin

clean:
	rm -rf $(TMP_PREFIX)

$(TMP_PREFIX)$(APP).bin: $(TMP_PREFIX)$(APP).elf
	$(CROSS_PREFIX)objcopy -O binary $^ $@
	$(CROSS_PREFIX)objdump -h $^ > $(TMP_PREFIX)$(APP).dis
	$(CROSS_PREFIX)objdump -d $^ >> $(TMP_PREFIX)$(APP).dis

$(TMP_PREFIX)$(APP).elf: $(SRCS)
	mkdir -p $(TMP_PREFIX)
	$(CROSS_PREFIX)gcc $(CCFLAGS) $(SRCS) -T $(LDSCRIPT) -o $@
