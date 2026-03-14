include ../project_paths.mk

# Default target
BOARD ?= s7

# Board-specific configurations
ifeq ($(BOARD),s7)
DOTF=$(PROJ_ROOT)soc/fpga/fpga_arty_s7.f
CONSTRAINTS_IO=constraints_io_s7.xdc
CONSTRAINTS_TIMING=constraints_timing_s7.xdc
BITFILE=fpga_arty_s7.bit
else
$(error Unknown BOARD: $(BOARD). Use  's7')
endif

FILES=$(shell HDL=$(HDL) $(SCRIPTS)/listfiles $(DOTF))
INCDIRS=$(shell HDL=$(HDL) $(SCRIPTS)/listfiles -f flati $(DOTF))

.PHONY: bit prog clean s7

# Default bit target
bit: $(BITFILE)

# Board-specific targets


s7:
	$(MAKE) BOARD=s7 bit

$(BITFILE): $(FILES) $(INCDIR) $(CONSTRAINTS_IO) $(CONSTRAINTS_TIMING) synth.tcl
	@echo "Building for board: $(BOARD)"
	@echo "set FILES {$(FILES)}" > filelist.tcl
	@echo "set INCDIRS {$(INCDIRS)}" >> filelist.tcl
	@echo "set CONSTRAINTS_IO {$(CONSTRAINTS_IO)}" >> filelist.tcl
	@echo "set CONSTRAINTS_TIMING {$(CONSTRAINTS_TIMING)}" >> filelist.tcl
	@echo "set BITFILE {$(BITFILE)}" >> filelist.tcl
	vivado -mode batch -source synth.tcl 2>&1 | tee synth_$(BOARD).log

prog: $(BITFILE)
	vivado -mode batch -source program.tcl

clean:
	rm -f *.dcp *.log *.jou *.bit *.rpt filelist.tcl
	rm -rf .Xil
