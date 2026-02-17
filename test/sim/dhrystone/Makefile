SRCS       := ../common/init_rv.S src/dhrystone_main.c src/dhrystone.c src/util.c
APP        := dhrystone
CCFLAGS    := -O3 -fno-inline -std=gnu99 -march=rv32im_zicsr -Wno-implicit-function-declaration -Wno-implicit-int -mabi=ilp32 -Os -ffreestanding -nostartfiles
TBEXEC     := ../tb_cxxrtl/tb_rv
MAX_CYCLES := 1000000

include ../common/src_only_app.mk
