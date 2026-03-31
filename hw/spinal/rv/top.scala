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
        //   outputs(0) -> CLINT       [0x0200_0000..0x0200_0FFF]  (4 KiB)
        //   outputs(1) -> APB bridge  [0x0201_0000..0x0201_FFFF]  (64 KiB) -> APB3 slaves:
        //                               0x0201_0000..0x0201_0FFF  UART
        //                               0x0201_1000..0x0201_1FFF  GPIO-A (buttons)
        //                               0x0201_2000..0x0201_2FFF  GPIO-B (LEDs)
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
                (BigInt("02000000", 16), BigInt(0x1000)),   // outputs(0): CLINT  at 0x0200_0000
                (BigInt("02010000", 16), BigInt(0x10000))   // outputs(1): APB bridge at 0x0201_0000
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
            rv.io.debug.dbg_exec_req   := dbgMod.io.core.dbg_exec_req
            rv.io.debug.dbg_exec_instr := dbgMod.io.core.dbg_exec_instr
            dbgMod.io.core.halted    := rv.io.debug.halted
            dbgMod.io.core.reg_rdata := rv.io.debug.reg_rdata
            dbgMod.io.core.csr_rdata := rv.io.debug.csr_rdata
            dbgMod.io.core.dbg_exec_done := rv.io.debug.dbg_exec_done
            dbgMod.io.core.dbg_exec_err  := rv.io.debug.dbg_exec_err
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
                                                 supportCompressed = true,
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
                                                 supportCompressed = true,
                                                 bootVector = BigInt("80000040", 16)))
    .setDefinitionName("RVXilinxTop"))  // rename generated module/file here
    .printPruned()
}

// ─────────────────────────────────────────────────────────────────────────────
// RVSim support: firmware build helpers and test-case definitions.
//
// RVSim simulates the FULL RVTop SoC (memory, CLINT, UART, GPIO) using
// SpinalHDL simulation.  Unlike the cxxrtl/verilator testbenches, all
// internal APB peripherals are live:
//   0x0200_0000..0x0200_0FFF  CLINT
//   0x0201_0000..0x0201_0FFF  UART
//   0x0201_1000..0x0201_1FFF  gpioA / buttons
//   0x0201_2000..0x0201_2FFF  gpioB / LEDs
//
// Usage:  sbt "runMain rv.RVSim [test]"
//   test = "base"  (default)
//   test = "fpga"
// ─────────────────────────────────────────────────────────────────────────────

/** Describes one firmware test: its source files, compile flags, and the
  * simulation body to run after the binary has been loaded into SRAM.
  *
  * @param name     short identifier shown in log output
  * @param srcs     source files relative to the workspace root (space-separated)
  * @param ccFlags  riscv32-unknown-elf-gcc flags (arch, ABI, optimisation …)
  * @param ldScript linker script path relative to workspace root
  * @param simBody  simulation procedure; receives the elaborated DUT and the
  *                 pre-loaded program image so it can inspect / drive I/Os
  */
case class RVSimTest(
  name     : String,
  srcs     : Seq[String],
  ccFlags  : String,
  ldScript : String,
  simBody  : (RVTop, Array[BigInt]) => Unit
)

/** Compile all sources for the given test into a flat binary.
  * Returns the program as an array of 32-bit little-endian words. */
object RVSimBuild {

  private val CC      = "riscv32-unknown-elf-gcc"
  private val OBJCOPY = "riscv32-unknown-elf-objcopy"
  private val INCDIR  = "test/sim/common"

  def build(test: RVSimTest): Array[BigInt] = {
    val tmpDir  = s"test/sim/${test.name}/tmp"
    new File(tmpDir).mkdirs()
    val elfFile = s"$tmpDir/${test.name}.elf"
    val binFile = s"$tmpDir/${test.name}.bin"
    val srcList = test.srcs.mkString(" ")

    val rc1 = s"$CC ${test.ccFlags} -I $INCDIR $srcList -T ${test.ldScript} -o $elfFile".!
    require(rc1 == 0, s"[${test.name}] Compilation failed (exit $rc1)")

    val rc2 = s"$OBJCOPY -O binary $elfFile $binFile".!
    require(rc2 == 0, s"[${test.name}] objcopy failed (exit $rc2)")

    loadBin(test.name, binFile)
  }

  private def loadBin(name: String, binFile: String): Array[BigInt] = {
    val bytes  = Files.readAllBytes(Paths.get(binFile))
    val nWords = (bytes.length + 3) / 4
    val words  = new Array[BigInt](nWords)
    for (i <- 0 until nWords) {
      var w = BigInt(0)
      for (b <- 0 until 4) {
        val idx = i * 4 + b
        w |= BigInt(if (idx < bytes.length) bytes(idx).toInt & 0xFF else 0) << (b * 8)
      }
      words(i) = w
    }
    println(s"[$name] loaded $nWords words (${bytes.length} bytes) from $binFile")
    words
  }
}

// ── Test catalogue ────────────────────────────────────────────────────────────

object RVSimTests {

  // Shared linker script used by all bare-metal tests
  private val LD_RV = "test/sim/common/memmap_rv.ld"

  // ── helpers ──────────────────────────────────────────────────────────────

  /** Pre-load SRAM, reset registers, start the clock. */
  def initDut(dut: RVTop, program: Array[BigInt]): Unit = {
    val memBank0 = dut.core.instrMem.mem.u_mem_bank0.u_mem
    val memBank1 = dut.core.instrMem.mem.u_mem_bank1.u_mem
    val memOffset = 16   // 0x80000040 → word 16
    for (i <- 0 until 4096) { memBank0.setBigInt(i, 0); memBank1.setBigInt(i, 0) }
    for (i <- 0 until program.length) {
      memBank0.setBigInt(memOffset + i, program(i))
      memBank1.setBigInt(memOffset + i, program(i))
    }
    for (j <- 0 until 32) dut.core.rv.RegFile.RegMem.setBigInt(j, 0)
    dut.clockDomain.forkStimulus(10)
    dut.io.irq       #= false
    dut.io.timer_irq #= false
  }

  // ── "base": register / memory dump after 60 instructions ─────────────────

  val base = RVSimTest(
    name    = "base",
    srcs    = Seq("test/sim/base/main.S"),
    ccFlags = "-march=rv32i -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles -Wl,--no-warn-rwx-segments",
    ldScript = LD_RV,
    simBody  = (dut, program) => {
      initDut(dut, program)
      var run = 60
      while (run > 0 && (dut.core.rv.exec.instr.toBigInt != 0 ||
                         (dut.core.rv.decoder.pc.toBigInt & 0xFFFFL) < 128)) {
        dut.clockDomain.waitSampling(1)
        run -= 1
        if (dut.core.rv.exec.valid.toBoolean) {
          printf("PC: %08X, INST: %08X",
            dut.core.rv.exec.pc.toBigInt, dut.core.rv.exec.instr.toBigInt)
          for (j <- 1 until 8)
            printf(" x%02d: %08X  ", j, dut.core.rv.RegFile.RegMem.getBigInt(j))
          println()
        }
      }
      println("Regfile at end of simu:")
      for (j <- 0 until 16 by 2) {
        printf("  x%02d: %08X   x%02d: %08X\n", j,
          dut.core.rv.RegFile.RegMem.getBigInt(j), j + 1,
          dut.core.rv.RegFile.RegMem.getBigInt(j + 1))
      }
      val memBank0 = dut.core.instrMem.mem.u_mem_bank0.u_mem
      
      if ((dut.core.rv.RegFile.RegMem.getBigInt(1) & 0xFFFFFL) == 0)
        println("[base] Test passed!")
      else
        println("[base] Test failed!")
    }
  )

  // ── "fpga": GPIO blink — LED[2:3] blink while button[0] held ─────────────
  // Phase 1: button released 400 cyc → led[3:2] must be 0
  // Phase 2: button pressed 2000 cyc → led[3:2] must toggle (BLINK_CLOG2=2)
  // Phase 3: button released 400 cyc → led[3:2] must return to 0

  val fpga = RVSimTest(
    name    = "fpga",
    srcs    = Seq("test/sim/fpga/src/start.S", "test/sim/fpga/src/main.c"),
    ccFlags = "-march=rv32im_zicsr -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles -Wl,--no-warn-rwx-segments -DBLINK_CLOG2=2",
    ldScript = LD_RV,
    simBody  = (dut, program) => {
      initDut(dut, program)
      dut.io.button.read #= 0

      dut.clockDomain.waitSampling(400)
      val ledsOff1 = (dut.io.led.write.toLong >> 2) & 3L
      println(s"[fpga] Phase1 (btn=0, 400 cyc):  led[3:2] = ${ledsOff1.toBinaryString}  (expect 0)")

      dut.io.button.read #= 1
      var ledHi = 0L; var ledLo = 0L
      for (_ <- 0 until 2000) {
        dut.clockDomain.waitSampling(1)
        val s = (dut.io.led.write.toLong >> 2) & 3L
        if (s != 0L) ledHi += 1 else ledLo += 1
      }
      println(s"[fpga] Phase2 (btn=1, 2000 cyc): led[3:2] high=$ledHi cyc, low=$ledLo cyc")

      dut.io.button.read #= 0
      dut.clockDomain.waitSampling(400)
      val ledsOff2 = (dut.io.led.write.toLong >> 2) & 3L
      println(s"[fpga] Phase3 (btn=0, 400 cyc):  led[3:2] = ${ledsOff2.toBinaryString}  (expect 0)")

      val blinkSeen = ledHi > 0 && ledLo > 0
      val pass = (ledsOff1 == 0L) && blinkSeen && (ledsOff2 == 0L)
      if (pass) println("[fpga] PASS — LEDs blinked while button pressed, off when released")
      else      println(s"[fpga] FAIL — ledsOff1=$ledsOff1 blinkSeen=$blinkSeen ledsOff2=$ledsOff2")
    }
  )

  // ── Registry: add new RVSimTest entries here ──────────────────────────────
  val all: Map[String, RVSimTest] = Seq(base, fpga).map(t => t.name -> t).toMap
}

// ─────────────────────────────────────────────────────────────────────────────
// Entry point
// Usage: sbt "runMain rv.RVSim [test]"
// ─────────────────────────────────────────────────────────────────────────────
object RVSim extends App {

  val testName = if (args.nonEmpty) args(0) else "base"
  val test     = RVSimTests.all.getOrElse(
    testName,
    { println(s"[RVSim] Unknown test '$testName'. Available: ${RVSimTests.all.keys.mkString(", ")}"); sys.exit(1) }
  )
  println(s"[RVSim] running test: $testName")

  val program = RVSimBuild.build(test)

  SimConfig
    .withFstWave
    .compile(new RVTop(config = RVConfig(
        supportFormal = false,
        supportMulDiv = false,
        InstAddrSize  = 32,
        dataAddrSize  = 32,
        bootVector    = BigInt("80000040", 16))))
    .doSim { dut => test.simBody(dut, program) }
}