APP          := coremark
MAX_CYCLES   := 100000000
CORE_DEBUG   ?= 0
ITERATIONS   ?= 1
CPU_HZ       ?= 1000000
VCD_FILE     ?= $(APP)_run.vcd

CROSS_PREFIX ?= riscv32-unknown-elf-
TBEXEC       ?= ../tb_cxxrtl/tb_rv
TBDIR        := $(dir $(abspath $(TBEXEC)))


###############################################################################

.SUFFIXES:
.PHONY: all run waves view bin tb clean clean_tb

all: run

RV_ROOT ?= ../../..
RV_GEN_CMD ?= sbt "runMain rv.RVVerilog"


rv:
	cd $(RV_ROOT) && $(RV_GEN_CMD)

run: $(APP).bin
	$(TBEXEC) --bin $(APP).bin --cycles $(MAX_CYCLES)

waves: $(APP).bin
	$(TBEXEC) --bin $(APP).bin --cycles $(MAX_CYCLES) --vcd $(VCD_FILE)

view: run
	gtkwave $(VCD_FILE)

bin: $(APP).bin

tb:
	$(MAKE) -C $(TBDIR) tb_rv

clean:
	rm -f $(APP).elf $(APP).bin $(APP).dis $(VCD_FILE)
	rm -rf dist/build/

clean_tb: clean
	$(MAKE) -C $(TBDIR) clean

###############################################################################

$(APP).bin: $(APP).elf
	$(CROSS_PREFIX)objcopy -O binary $^ $@
	$(CROSS_PREFIX)objdump -h $(APP).elf > $(APP).dis
	$(CROSS_PREFIX)objdump -d $(APP).elf >> $(APP).dis

$(APP).elf:
	make -C dist ITERATIONS=$(ITERATIONS) XCFLAGS="-DEE_TICKS_PER_SEC=$(CPU_HZ) $(if $(filter-out 0,$(CORE_DEBUG)),-DCORE_DEBUG=$(CORE_DEBUG),)"
	cp dist/build/coremark.elf $(APP).elf
