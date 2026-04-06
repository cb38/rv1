# ─────────────────────────────────────────────────────────────────────────
# Makefile — Build Linux for RV1 simulation
#
# Targets:
#   make docker    — build the Docker image (one-time)
#   make linux     — build kernel+initramfs+dtb → out/linux.bin
#   make run       — run linux.bin in the cxxrtl testbench
#   make clean     — remove build artifacts
#
# Prerequisites:
#   - Docker
#   - dtc (device-tree-compiler)  — brew install dtc
#   - tb_rv testbench already built (make tb_build from test/sim/)
# ─────────────────────────────────────────────────────────────────────────

DOCKER_IMAGE  := rv1-linux-builder
TB            := ../tb_cxxrtl/tb_rv
MAX_CYCLES    ?= 500000000
OUT           := out

.PHONY: all docker linux run clean

all: linux

# ── Docker image (one-time, ~5 min) ──────────────────────────────────────
docker:
	docker build -t $(DOCKER_IMAGE) .

# ── Build kernel + initramfs + DTB inside Docker ────────────────────────
linux:
	mkdir -p $(OUT)
	docker run --rm \
		--platform linux/amd64 \
		-v $(PWD)/docker-build.sh:/build/docker-build.sh \
		-v $(PWD)/rv1_defconfig_fragment:/build/rv1_defconfig_fragment \
		-v $(PWD)/rv1.dts:/build/rv1.dts \
		-v $(PWD)/$(OUT):/out \
		$(DOCKER_IMAGE) bash /build/docker-build.sh
	@echo ""
	@echo "Build output in $(OUT)/:"
	@ls -lh $(OUT)/linux.bin $(OUT)/Image $(OUT)/rv1.dtb
	@echo ""

# ── Run in testbench ────────────────────────────────────────────────────
run: $(OUT)/linux.bin $(OUT)/build_info.txt
	$(eval DTB_ADDR := $(shell grep DTB_ADDR $(OUT)/build_info.txt | cut -d= -f2))
	$(TB) --bin $(OUT)/linux.bin --noshift --bootpc 0x80000000 --dtb-addr $(DTB_ADDR) \
		--cycles $(MAX_CYCLES) --logfile $(OUT)/boot.log 2>&1 | tee $(OUT)/boot_stderr.log
	@echo ""
	@grep -E "Machine model|Run /init|boot successful|Hello from|Kernel panic" $(OUT)/boot.log | head -10

# ── Clean ───────────────────────────────────────────────────────────────
clean:
	rm -rf $(OUT)
