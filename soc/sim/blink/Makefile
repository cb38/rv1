# Build the blink test firmware binary.
# RVSim (sbt "runMain rv.RVSim blink") will compile and load this automatically.
# Use this Makefile only to produce the .bin for standalone inspection.

SRCS       := src/start.S src/main.c
APP        := blink
CCFLAGS    := -march=rv32im_zicsr -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles -Wl,--no-warn-rwx-segments
ifeq ($(DEBUG),1)
CCFLAGS    += -g -Og
endif
LDSCRIPT   := ../common/memmap.ld

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

$(TMP_PREFIX)$(APP).elf: $(SRCS) $(wildcard src/*.h)
	mkdir -p $(TMP_PREFIX)
	$(CROSS_PREFIX)gcc $(CCFLAGS) $(SRCS) -T $(LDSCRIPT) -o $@
