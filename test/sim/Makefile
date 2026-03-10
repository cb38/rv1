# Run all simulation tests
# Usage:
#   make          - build RTL + testbench, run all tests
#   make run      - run all tests (assumes RTL + TB already built)
#   make rv       - generate Verilog only
#   make tb_build - build testbench only

TESTS := base mul_hw irq_csr compressed_c hellow debug

RV_ROOT    := ../..
RV_GEN_CMD := sbt "runMain rv.RVVerilog"

# Expected exit codes per test (printed by tb, not process return code)
EXPECT_mul_hw       := 0
EXPECT_irq_csr      := 0
EXPECT_compressed_c := 0
EXPECT_hellow       := 0
EXPECT_debug        := 0
EXPECT_base         := 0

.PHONY: all rv tb_build run clean $(TESTS)

all: rv tb_build run

rv:
	cd $(RV_ROOT) && $(RV_GEN_CMD)
 
tb_build:
	$(MAKE) -C tb_cxxrtl clean
	$(MAKE) -C tb_cxxrtl

run: $(TESTS)
	@echo ""
	@echo "========================================"
	@echo " All $(words $(TESTS)) tests passed."
	@echo "========================================"

$(TESTS):
	@echo "----------------------------------------"
	@echo " TEST: $@"
	@echo "----------------------------------------"
	@$(MAKE) -C $@ run 2>&1 | tee $@/.test_output.tmp; \
	EXIT_CODE=$$(sed -n 's/.*Exit code \([0-9]*\).*/\1/p' $@/.test_output.tmp | tail -1); \
	TIMED_OUT=$$(grep -c 'Max cycles reached' $@/.test_output.tmp); \
	rm -f $@/.test_output.tmp; \
	if [ "$$TIMED_OUT" != "0" ]; then \
		echo "[FAIL] $@ (timed out - max cycles reached)"; \
		exit 1; \
	elif [ "$$EXIT_CODE" = "$(EXPECT_$@)" ]; then \
		echo "[PASS] $@ (exit code $$EXIT_CODE)"; \
	else \
		echo "[FAIL] $@ (got exit code $$EXIT_CODE, expected $(EXPECT_$@))"; \
		exit 1; \
	fi

clean:
	@for t in $(TESTS); do $(MAKE) -C $$t clean 2>/dev/null; done
	$(MAKE) -C tb_cxxrtl clean 2>/dev/null
