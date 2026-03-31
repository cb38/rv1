target extended-remote localhost:3333
monitor reset halt
file test/sim/fpga/tmp/fpga.elf
load
compare-sections