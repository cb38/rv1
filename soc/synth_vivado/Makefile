include ../project_paths.mk

# Default target
BOARD ?= s7

# Vivado runtime selection
VIVADO_USE_DOCKER ?= 1
VIVADO_BIN ?= vivado

# Docker settings for Vivado runtime
DOCKER ?= docker
VIVADO_DOCKER_IMAGE ?= xilinx-vivado:2025.2
VIVADO_DOCKER_PLATFORM ?= linux/amd64
VIVADO_DOCKER_USER ?= $(shell id -u):$(shell id -g)
VIVADO_DOCKER_SRC_HOST_PATH ?= $(abspath ../..)
VIVADO_DOCKER_SRC_PATH ?= /src
VIVADO_DOCKER_WORK_PATH ?= /work
VIVADO_DOCKER_HOME ?= /work
VIVADO_DOCKER_WORKDIR ?= /work
VIVADO_SETTINGS ?= /opt/Xilinx/2025.2/Vivado/settings64.sh
VIVADO_LD_PRELOAD ?= /lib/x86_64-linux-gnu/libudev.so.1 
#VIVADO_LD_PRELOAD ?= /opt/udev_stub.so


ifeq ($(VIVADO_USE_DOCKER),1)
define RUN_VIVADO
$(DOCKER) run --platform $(VIVADO_DOCKER_PLATFORM) --interactive --tty --rm \
	-u $(VIVADO_DOCKER_USER) \
	-v $(VIVADO_DOCKER_SRC_HOST_PATH):$(VIVADO_DOCKER_SRC_PATH):rw \
	-v $(CURDIR):$(VIVADO_DOCKER_WORK_PATH):rw \
	-e HOME=$(VIVADO_DOCKER_HOME) \
	-e DISPLAY= \
	-e _JAVA_AWT_WM_NONREPARENTING=1 \
	-e XILINX_LOCAL_USER_DATA=no \
	$(VIVADO_DOCKER_IMAGE) /bin/bash -lc 'export LD_PRELOAD=$(VIVADO_LD_PRELOAD) && source $(VIVADO_SETTINGS) && cd $(VIVADO_DOCKER_WORKDIR) && $(VIVADO_BIN) $(1)'
endef
else
define RUN_VIVADO
$(VIVADO_BIN) $(1)
endef
endif

# Board-specific configurations
ifeq ($(BOARD),s7)
DOTF=$(PROJ_ROOT)soc/fpga/fpga_arty_s7.f
CONSTRAINTS_IO=constraints_io_s7.xdc
CONSTRAINTS_TIMING=constraints_timing_s7.xdc
BITFILE=fpga_arty_s7.bit
SYNTH_TCL=synth_s7.tcl
else
$(error Unknown BOARD: $(BOARD). Use  's7')
endif

FILES=$(shell HDL=$(HDL) $(SCRIPTS)/listfiles $(DOTF))
INCDIRS=$(shell HDL=$(HDL) $(SCRIPTS)/listfiles -f flati $(DOTF))

ifeq ($(VIVADO_USE_DOCKER),1)
FILES_VIVADO=$(patsubst $(VIVADO_DOCKER_SRC_HOST_PATH)%,$(VIVADO_DOCKER_SRC_PATH)%,$(FILES))
INCDIRS_VIVADO=$(patsubst $(VIVADO_DOCKER_SRC_HOST_PATH)%,$(VIVADO_DOCKER_SRC_PATH)%,$(INCDIRS))
else
FILES_VIVADO=$(FILES)
INCDIRS_VIVADO=$(INCDIRS)
endif

.PHONY: bit prog clean s7 debug_paths interactive_bash

# Default bit target
bit: $(BITFILE)

# Board-specific targets


s7:
	$(MAKE) BOARD=s7 bit

debug_paths:
	@echo "VIVADO_USE_DOCKER=$(VIVADO_USE_DOCKER)"
	@echo "VIVADO_DOCKER_SRC_HOST_PATH=$(VIVADO_DOCKER_SRC_HOST_PATH)"
	@echo "VIVADO_DOCKER_SRC_PATH=$(VIVADO_DOCKER_SRC_PATH)"
	@echo "CURDIR=$(CURDIR)"
	@echo "VIVADO_DOCKER_WORK_PATH=$(VIVADO_DOCKER_WORK_PATH)"
	@echo "DOTF=$(DOTF)"
	@echo "FILES=$(FILES)"
	@echo "FILES_VIVADO=$(FILES_VIVADO)"
	@echo "INCDIRS=$(INCDIRS)"
	@echo "INCDIRS_VIVADO=$(INCDIRS_VIVADO)"

$(BITFILE): $(FILES) $(INCDIRS) $(CONSTRAINTS_IO) $(CONSTRAINTS_TIMING) $(SYNTH_TCL)
	@echo "Building for board: $(BOARD)"
	@echo "set FILES {$(FILES_VIVADO)}" > filelist.tcl
	@echo "set INCDIRS {$(INCDIRS_VIVADO)}" >> filelist.tcl
	@echo "set CONSTRAINTS_IO {$(CONSTRAINTS_IO)}" >> filelist.tcl
	@echo "set CONSTRAINTS_TIMING {$(CONSTRAINTS_TIMING)}" >> filelist.tcl
	@echo "set BITFILE {$(BITFILE)}" >> filelist.tcl
	$(call RUN_VIVADO,-mode batch -source $(SYNTH_TCL)) 2>&1 | tee synth_$(BOARD).log

prog: $(BITFILE)
	$(call RUN_VIVADO,-mode batch -source program.tcl)

clean:
	rm -f *.dcp *.log *.jou  *.rpt filelist.tcl *.bit
	rm -rf .Xil

# Ouvre un shell interactif dans le conteneur Docker Vivado
interactive_bash:
ifeq ($(VIVADO_USE_DOCKER),1)
	$(DOCKER) run --platform $(VIVADO_DOCKER_PLATFORM) --interactive --tty --rm \
		-u $(VIVADO_DOCKER_USER) \
		-v $(VIVADO_DOCKER_SRC_HOST_PATH):$(VIVADO_DOCKER_SRC_PATH):rw \
		-v $(CURDIR):$(VIVADO_DOCKER_WORK_PATH):rw \
		-e HOME=$(VIVADO_DOCKER_HOME) \
		-e DISPLAY= \
		-e _JAVA_AWT_WM_NONREPARENTING=1 \
		-e XILINX_LOCAL_USER_DATA=no \
		$(VIVADO_DOCKER_IMAGE) /bin/bash
else
	@echo "[ERROR] VIVADO_USE_DOCKER=0 : Pas de conteneur Docker."
endif
