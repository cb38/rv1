package rv
import spinal.core._
import spinal.lib._ 
import rv.bus.axi4lite._
import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.misc.pipeline._
import scala.io.Source
import scala.sys.process._
import java.io.File
import java.nio.file.{Files, Paths}
import multiport_memory._
import rv.bus.axi4lite._
import spinal.lib.com.jtag.Jtag
import spinal.lib.com.uart.{Apb3UartCtrl, Uart, UartCtrlGenerics, UartCtrlMemoryMappedConfig}
import spinal.lib.bus.amba3.apb._
import spinal.lib.io.TriStateArray

object RVVerilog extends App {
  val config=SpinalConfig(
    device=Device.XILINX,
    targetDirectory = "hw/gen",
    mergeAsyncProcess = true,
    defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW)
  )
 
  config.generateVerilog(new RV(config = RVConfig(supportFormal = false,
                                                 supportMulDiv = true,
                                                 supportCompressed = true,
                                                 supportZbs = true,
                                                 supportZba = true,
                                                 supportZbb = true,
                                                 supportZbkb = true,
                                                 supportDebug = true,
                                                 bootVector = BigInt("80000040", 16)))).printPruned()
  config.generateVerilog(new DebugModule).printPruned()
  config.generateVerilog(new JtagDTM).printPruned()
}

// Formal verification target with ALTOPS (replaces real mul/div with XOR-based ops)
object RVFormalVerilog extends App {
  val config=SpinalConfig(
    device=Device.XILINX,
    targetDirectory = "hw/gen",
    mergeAsyncProcess = true,
    defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW)
  )
 
  config.generateVerilog(new RV(config = RVConfig(supportFormal = true,
                                                 supportMulDiv = true,
                                                 supportCompressed = true,
                                                 supportZbs = true,
                                                 supportZba = true,
                                                 supportZbb = true,
                                                 supportZbkb = true,
                                                 supportDebug = true,
                                                 bootVector = BigInt("80000040", 16))).setDefinitionName("RV_formal")).printPruned()
}

// Top-level SoC wrapping the RV core with memories, CLINT, and UART.
// Memory map (CPU instr_axi and data_axi ports):
//  0x8000_0000 .. 0x8000_3FFF  Unified instr+data memory (16 KiB, dual-ported AXI4Lite)    
// Peripheral address map (CPU periph_axi port, addr[31:16] == 0x0200):
//   0x0200_0000 .. 0x0200_0FFF  CLINT  (4 KiB)
//   0x0201_0000 .. 0x0201_FFFF  APB   (64 KiB, via AXI4Lite->APB3 bridge)
class RVTop(config: RVConfig) extends Component {
    val io = new Bundle {
   
        val button      = master(TriStateArray(4 bits))
        val led      = master(TriStateArray(4 bits))
        val irq       = in(Bool)
        val timer_irq = in(Bool)
        val uart      = master(Uart())
        // JTAG debug port — only present when supportDebug=true AND useXilinxJtag=false.
        // When useXilinxJtag=true the DTM uses BSCANE2 internals; no external JTAG pins.
        val jtag_tck    = if(config.hasDebug && !config.useXilinxJtag) in(Bool)  else null
        val jtag_tms    = if(config.hasDebug && !config.useXilinxJtag) in(Bool)  else null
        val jtag_tdi    = if(config.hasDebug && !config.useXilinxJtag) in(Bool)  else null
        val jtag_tdo    = if(config.hasDebug && !config.useXilinxJtag) out(Bool) else null
        val jtag_trst_n = if(config.hasDebug && !config.useXilinxJtag) in(Bool)  else null
    }

    val core = new Area {

        val rv = new RV(config)
        val cfg = MemConfig(memorySize = 16*1024, dataWidth = 32)

        // Unified instruction + data memory.
        // io.instr_axi = read-only (CPU fetch); io.data_axi = full access (CPU data + debugger code upload).
        val instrMem = new AXI4Lite_DualPort_Mem(
            Axi4LiteConfig(addressWidth = config.InstAddrSize, dataWidth = 32), cfg)

        rv.io.instr_axi <> instrMem.io.instr_axi
        rv.io.data_axi  <> instrMem.io.data_axi
        
        // -----------------------------------------------------------------------
        // Peripheral crossbar: periph_axi -> Axi4LiteDecoder -> slaves
        //   outputs(0) -> CLINT       [0x0_0000..0x0_0FFF]
        //   outputs(1) -> APB bridge  [0x1_0000..0x1_FFFF] -> APB3 slaves (UART, GPIO, etc.)
        // -----------------------------------------------------------------------
        val clint = new CLINT(config.dataAddrSize)

        val apbBridge = new Axi4LiteToApb3Bridge(
            axiAddrWidth = config.dataAddrSize,
            apbAddrWidth = 16,   
            dataWidth    = 32
        )
        val gpioACtrl = Apb3Gpio(
            gpioWidth = 4,
            withReadSync = true
            )
        val gpioBCtrl = Apb3Gpio(
            gpioWidth = 4,
            withReadSync = true
            )    
    
        val uartCtrl = Apb3UartCtrl(UartCtrlMemoryMappedConfig(
            uartCtrlConfig = UartCtrlGenerics(
                dataWidthMax      = 8,
                clockDividerWidth = 20,
                preSamplingSize   = 1,
                samplingSize      = 5,
                postSamplingSize  = 2
            ),
            txFifoDepth = 16,
            rxFifoDepth = 16
        ))
        val apbDecoder =  Apb3Decoder(
             master = apbBridge.io.apb,
             slaves = List(
                uartCtrl.io.apb  -> (0x0000, 4 KiB),
                gpioACtrl.io.apb -> (0x1000, 4 KiB),
                gpioBCtrl.io.apb -> (0x2000, 4 KiB)
         )
        )

        // connect to pins 
        uartCtrl.io.uart  <> io.uart
        gpioACtrl.io.gpio  <> io.button
        io.led <> gpioBCtrl.io.gpio

      

        val periphXbar = new Axi4LiteDecoder(
            axiConfig = Axi4LiteConfig(addressWidth = config.dataAddrSize, dataWidth = 32),
            addrMap   = Seq(
                (BigInt(0x00000), BigInt(0x1000)),  // outputs(0): CLINT
                (BigInt(0x10000), BigInt(0x10000))   // outputs(1): APB bridge -> UART
            )
        )
        rv.io.periph_axi         <> periphXbar.io.input
        periphXbar.io.outputs(0) <> clint.io.bus
        periphXbar.io.outputs(1) <> apbBridge.io.axi

        rv.io.irq       := io.irq
        rv.io.timer_irq := clint.io.timerIrq || io.timer_irq
        rv.io.soft_irq  := clint.io.softIrq

        // -----------------------------------------------------------------------
        // Debug: DTM -> DebugModule -> rv.io.debug
        //
        // Two DTM variants:
        //   useXilinxJtag=false (default / simulation):
        //     JtagDTM — software TAP with explicit tck/tms/tdi/tdo/trst_n pins
        //   useXilinxJtag=true (Xilinx 7-series FPGA):
        //     Xilinx7JtagDtm — BSCANE2-based TAP, no external JTAG pins needed.
        //     ApbDmiBridge adapts its APB master output to DebugModule's DmiBus.
        // -----------------------------------------------------------------------
        if (config.hasDebug) {
            val dbgMod = new DebugModule()

            if (config.useXilinxJtag) {
                val dtm    = new Xilinx7JtagDtm()
                val bridge = new ApbDmiBridge()

                dtm.io.clk_dmi   := ClockDomain.current.readClockWire
                dtm.io.rst_n_dmi := ClockDomain.current.readResetWire

                bridge.io.psel    := dtm.io.dmi_psel
                bridge.io.penable := dtm.io.dmi_penable
                bridge.io.pwrite  := dtm.io.dmi_pwrite
                bridge.io.paddr   := dtm.io.dmi_paddr
                bridge.io.pwdata  := dtm.io.dmi_pwdata
                dtm.io.dmi_prdata  := bridge.io.prdata
                dtm.io.dmi_pready  := bridge.io.pready
                dtm.io.dmi_pslverr := bridge.io.pslverr

                bridge.io.dmi <> dbgMod.io.dmi
            } else {
                val dtm = new JtagDTM()
                dtm.io.tck    := io.jtag_tck
                dtm.io.tms    := io.jtag_tms
                dtm.io.tdi    := io.jtag_tdi
                io.jtag_tdo   := dtm.io.tdo
                dtm.io.trst_n := io.jtag_trst_n
                dtm.io.dmi <> dbgMod.io.dmi
            }

            rv.io.debug.halt_req   := dbgMod.io.core.halt_req
            rv.io.debug.resume_req := dbgMod.io.core.resume_req
            rv.io.debug.reg_addr   := dbgMod.io.core.reg_addr
            rv.io.debug.reg_wdata  := dbgMod.io.core.reg_wdata
            rv.io.debug.reg_wr     := dbgMod.io.core.reg_wr
            rv.io.debug.csr_addr   := dbgMod.io.core.csr_addr
            rv.io.debug.csr_wdata  := dbgMod.io.core.csr_wdata
            rv.io.debug.csr_wr     := dbgMod.io.core.csr_wr
            dbgMod.io.core.halted    := rv.io.debug.halted
            dbgMod.io.core.reg_rdata := rv.io.debug.reg_rdata
            dbgMod.io.core.csr_rdata := rv.io.debug.csr_rdata
        }
    }
}


object RVTopVerilog extends App {
  val config=SpinalConfig(
    device=Device.XILINX,
    targetDirectory = "hw/gen",
    mergeAsyncProcess = true,
    defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW)
  )
 
  config.generateVerilog(new RVTop(config = RVConfig(supportFormal = false,
                                                 supportMulDiv = false,
                                                 InstAddrSize = 32,
                                                 dataAddrSize = 32,
                                                 supportZbs = true,
                                                 supportZba = true,
                                                 supportZbb = true,
                                                 supportZbkb = true,
                                                 supportDebug = true,
                                                 bootVector = BigInt("00000000", 16)))).printPruned()
}

// Generates RVTop with the BSCANE2-based Xilinx JTAG DTM.
// No external JTAG pins; connect GDB via OpenOCD's 'xilinx_bscan_sysfs' or
// 'jtagspi' target on the Xilinx USB cable (hw_server / xvc_server).
object RVTopXilinxVerilog extends App {
  val config=SpinalConfig(
    device=Device.XILINX,
    targetDirectory = "hw/gen",
    mergeAsyncProcess = true,
    defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW)
  )

  config.generateVerilog(new RVTop(config = RVConfig(supportFormal = false,
                                                 supportMulDiv = false,
                                                 InstAddrSize = 32,
                                                 dataAddrSize = 32,
                                                 supportZbs = true,
                                                 supportZba = true,
                                                 supportZbb = true,
                                                 supportZbkb = true,
                                                 supportDebug = true,
                                                 useXilinxJtag = true,
                                                 bootVector = BigInt("80000040", 16)))).printPruned()
}

object RVSim extends App {

  // Assemble and link test/sim/base/main.S
  val baseDir = "test/sim/base"
  val tmpDir  = s"$baseDir/tmp"
  new File(tmpDir).mkdirs()
  val ccFlags  = "-march=rv32i -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles -Wl,--no-warn-rwx-segments"
  val ldScript = s"$baseDir/memmap_rv.ld"
  val elfFile  = s"$tmpDir/main.elf"
  val binFile  = s"$tmpDir/main.bin"

  val rc1 = s"riscv32-unknown-elf-gcc $ccFlags $baseDir/main.S -T $ldScript -o $elfFile".!
  require(rc1 == 0, s"Assembly failed with exit code $rc1")
  val rc2 = s"riscv32-unknown-elf-objcopy -O binary $elfFile $binFile".!
  require(rc2 == 0, s"objcopy failed with exit code $rc2")

  // Load binary as array of 32-bit words (little-endian)
  val binBytes = Files.readAllBytes(Paths.get(binFile))
  val nWords   = (binBytes.length + 3) / 4
  val program  = new Array[BigInt](nWords)
  for (i <- 0 until nWords) {
    var word = BigInt(0)
    for (b <- 0 until 4) {
      val idx = i * 4 + b
      val byteVal = if (idx < binBytes.length) (binBytes(idx).toInt & 0xFF) else 0
      word |= BigInt(byteVal) << (b * 8)
    }
    program(i) = word
  }
  println(s"Loaded $nWords words from $binFile (${binBytes.length} bytes)")
   

   SimConfig
    .withFstWave
   //withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW)))
    .compile(new RVTop(config = RVConfig(supportFormal = false,
                                                 supportMulDiv = false,
                                                 InstAddrSize = 32,
                                                 dataAddrSize = 32,
                                                 bootVector = BigInt("80000040", 16)) )).doSim{ dut =>
  // MultiPortMem_1w_2rs has two physical banks kept in sync by identical writes.
  // Pre-load both so instr_axi (bank0) and data_axi (bank1) see the program.
  val memBank0 = dut.core.instrMem.mem.u_mem_bank0.u_mem
  val memBank1 = dut.core.instrMem.mem.u_mem_bank1.u_mem
  // Binary starts at _start (0x80000040), which maps to word offset 16 in memory
  val memOffset = 16
  // Clear all memory first
  for (i <- 0 until 1024) {
            memBank0.setBigInt(i, 0)
            memBank1.setBigInt(i, 0)
        }
  // Load program at correct offset
  for (i <- 0 until program.length) {
            memBank0.setBigInt(memOffset + i, program(i))
            memBank1.setBigInt(memOffset + i, program(i))
        }
  dut.clockDomain.forkStimulus(10)
  dut.io.irq #= false
  dut.io.timer_irq #= false
  var run = 60
  // init Regfile
  for (j <- 0 until 32 ) {
    dut.core.rv.RegFile.RegMem.setBigInt(j,0)
    println()
  }
  // run simulation and stop when instruction is 0
   while(run>0 && (dut.core.rv.exec.instr.toBigInt != 0 ||  (dut.core.rv.decoder.pc.toBigInt & 0xFFFF) < 128)) {
        dut.clockDomain.waitSampling(1)
        run -= 1
        if (dut.core.rv.exec.valid.toBoolean)  {
             printf("PC: %08X, INST: %08X",dut.core.rv.exec.pc.toBigInt, dut.core.rv.exec.instr.toBigInt)
             // print status of reg 0-7 
                for (j <- 1 until 8 ) {
                    printf(" x%02d: %08X  ", j,dut.core.rv.RegFile.RegMem.getBigInt(j))
                }
             println()
        }
    }
  // read value from regfile
  println("value of Regfile at the  end of simu") 
  for (j <- 0 until 16 by 2) {
    printf("x%02d:  %08X   x%02d:  %08X ", j,dut.core.rv.RegFile.RegMem.getBigInt(j),j+1,dut.core.rv.RegFile.RegMem.getBigInt(j+1))
    println()
  }
  // read value from memory
    println("value of memory at the  end of simu")
    for (j <- 32 until 60 by 2) {
        printf("mem[%02d]:  %08X   mem[%02d]:  %08X ", j,memBank0.getBigInt(j),j+1,memBank0.getBigInt(j+1))
        println()  
    }
        // check x1 is 0
    if((dut.core.rv.RegFile.RegMem.getBigInt(1) & 0xFFFFF) == 0){
        println("Test passed!")
    } else {
        println("Test failed!") 
    }   
    
 }
}