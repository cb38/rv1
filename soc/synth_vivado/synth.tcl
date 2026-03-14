source filelist.tcl

set PART xc7a100tcsg324-1
set TOP fpga

proc checkpoint_and_report {stage} {
	write_checkpoint -force ${stage}.dcp
	report_timing_summary -file ${stage}_timing.rpt
	report_utilization -file ${stage}_util.rpt
	report_timing_summary -no_detailed_paths
}

add_files $FILES
read_xdc constraints_timing.xdc

synth_design -include_dirs $INCDIRS -part $PART -top $TOP \
	-directive PerformanceOptimized
checkpoint_and_report synth

read_xdc constraints_io.xdc
place_design -directive Explore
checkpoint_and_report place

route_design -directive Explore
phys_opt_design -directive Explore
checkpoint_and_report route

write_bitstream -force fpga.bit
