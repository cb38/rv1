package rv

import spinal.core._
import spinal.lib._
import rv.bus.axi4lite._
import spinal.core.sim._
import spinal.sim._
import spinal.lib.com.uart.{Uart, UartCtrlGenerics, UartCtrlMemoryMappedConfig, Apb3UartCtrl}
import spinal.lib.bus.amba3.apb._
import spinal.lib.io.TriStateArray
import multiport_memory._
import spinal.lib.fsm._

// =============================================================================
// RVDebugSimTop
//
// Simulation-only SoC variant: identical to RVTop but exposes the DMI bus
// directly at the top level (no JTAG DTM layer).
// This lets the testbench drive DMI transactions in simulation without a
// JTAG bit-bang layer, giving clean waveforms.
// =============================================================================
class RVDebugSimTop(rvConfig: RVConfig) extends Component {
  val io = new Bundle {
    val button    = master(TriStateArray(4 bits))
    val led       = master(TriStateArray(4 bits))
    val irq       = in(Bool)
    val timer_irq = in(Bool)
    val uart      = master(Uart())
    // DMI bus exposed for direct testbench access
    val dmi = slave(DmiBus())
  }

  val core = new Area {
    val rv  = new RV(rvConfig)
    val cfg = MemConfig(memorySize = 16 * 1024, dataWidth = 32)

    val instrMem = new AXI4Lite_DualPort_Mem(
      Axi4LiteConfig(addressWidth = rvConfig.InstAddrSize, dataWidth = 32), cfg)
    rv.io.instr_axi <> instrMem.io.instr_axi
    rv.io.data_axi  <> instrMem.io.data_axi

    val clint      = new CLINT(rvConfig.dataAddrSize)
    val apbBridge  = new Axi4LiteToApb3Bridge(
      axiAddrWidth = rvConfig.dataAddrSize, apbAddrWidth = 16, dataWidth = 32)
    val gpioACtrl  = Apb3Gpio(gpioWidth = 4, withReadSync = true)
    val gpioBCtrl  = Apb3Gpio(gpioWidth = 4, withReadSync = true)
    val uartCtrl   = Apb3UartCtrl(UartCtrlMemoryMappedConfig(
      uartCtrlConfig = UartCtrlGenerics(
        dataWidthMax      = 8,
        clockDividerWidth = 20,
        preSamplingSize   = 1,
        samplingSize      = 5,
        postSamplingSize  = 2),
      txFifoDepth = 16,
      rxFifoDepth = 16))

    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = List(
        uartCtrl.io.apb  -> (0x0000, 4 KiB),
        gpioACtrl.io.apb -> (0x1000, 4 KiB),
        gpioBCtrl.io.apb -> (0x2000, 4 KiB)))

    uartCtrl.io.uart <> io.uart
    gpioACtrl.io.gpio <> io.button
    io.led <> gpioBCtrl.io.gpio

    val periphXbar = new Axi4LiteDecoder(
      axiConfig = Axi4LiteConfig(addressWidth = rvConfig.dataAddrSize, dataWidth = 32),
      addrMap   = Seq(
        (BigInt("02000000", 16), BigInt(0x1000)),
        (BigInt("02010000", 16), BigInt(0x10000))))
    rv.io.periph_axi         <> periphXbar.io.input
    periphXbar.io.outputs(0) <> clint.io.bus
    periphXbar.io.outputs(1) <> apbBridge.io.axi

    rv.io.irq       := io.irq
    rv.io.timer_irq := clint.io.timerIrq || io.timer_irq
    rv.io.soft_irq  := clint.io.softIrq

    // ---- Debug module wired to the exposed DMI port (no JTAG layer) ----
    val dbgMod = new DebugModule()
    dbgMod.io.dmi <> io.dmi

    rv.io.debug.halt_req         := dbgMod.io.core.halt_req
    rv.io.debug.resume_req       := dbgMod.io.core.resume_req
    rv.io.debug.reg_addr         := dbgMod.io.core.reg_addr
    rv.io.debug.reg_wdata        := dbgMod.io.core.reg_wdata
    rv.io.debug.reg_wr           := dbgMod.io.core.reg_wr
    rv.io.debug.csr_addr         := dbgMod.io.core.csr_addr
    rv.io.debug.csr_wdata        := dbgMod.io.core.csr_wdata
    rv.io.debug.csr_wr           := dbgMod.io.core.csr_wr
    rv.io.debug.dbg_exec_req     := dbgMod.io.core.dbg_exec_req
    rv.io.debug.dbg_exec_instr   := dbgMod.io.core.dbg_exec_instr
    dbgMod.io.core.halted        := rv.io.debug.halted
    dbgMod.io.core.reg_rdata     := rv.io.debug.reg_rdata
    dbgMod.io.core.csr_rdata     := rv.io.debug.csr_rdata
    dbgMod.io.core.dbg_exec_done := rv.io.debug.dbg_exec_done
    dbgMod.io.core.dbg_exec_err  := rv.io.debug.dbg_exec_err
  }
}

// =============================================================================
// DmiDriver
//
// Simulation helper: drives DMI bus transactions directly.
// One write or read = 2 clock cycles (req cycle + resp cycle).
// =============================================================================
class DmiDriver(dmi: DmiBus, cd: ClockDomain) {

  // ---- Idle defaults ----
  dmi.req_valid #= false
  dmi.req_addr  #= 0
  dmi.req_data  #= 0
  dmi.req_op    #= 0

  // ------------------------------------------------------------------------
  // Low-level DMI primitives
  // ------------------------------------------------------------------------

  def write(addr: Int, data: BigInt): Unit = {
    dmi.req_valid #= true
    dmi.req_addr  #= addr
    dmi.req_data  #= data
    dmi.req_op    #= 2         // write
    cd.waitSampling()
    dmi.req_valid #= false
    cd.waitSampling()          // DebugModule registers resp on this clock
  }

  def read(addr: Int): BigInt = {
    dmi.req_valid #= true
    dmi.req_addr  #= addr
    dmi.req_data  #= 0
    dmi.req_op    #= 1         // read
    cd.waitSampling()
    dmi.req_valid #= false
    cd.waitSampling()          // DebugModule registers resp
    dmi.resp_data.toBigInt
  }

  // ------------------------------------------------------------------------
  // Higher-level helpers that mirror OpenOCD's behaviour
  // ------------------------------------------------------------------------

  /** Poll abstractcs until busy=0.  Returns false on timeout. */
  def waitNotBusy(timeout: Int = 5000): Boolean = {
    var t = timeout
    while (t > 0) {
      val cs = read(0x16)      // DMI_ABSTRACTCS
      val busy = (cs >> 12) & 1
      val err  = (cs >> 8) & 7
      if (err != 0) {
        println(f"[DMI] cmderr=${err}%d while waiting not-busy (abstractcs=0x${cs}%08X)")
        return false
      }
      if (busy == 0) return true
      t -= 1
    }
    println(s"[DMI] TIMEOUT waiting for busy=0")
    false
  }

  /** Poll dmstatus until allhalted=1.  Returns false on timeout. */
  def waitHalted(timeout: Int = 5000): Boolean = {
    var t = timeout
    while (t > 0) {
      val st = read(0x11)      // DMI_DMSTATUS
      if (((st >> 9) & 1) == 1) return true  // allhalted bit 9
      t -= 1
    }
    println(s"[DMI] TIMEOUT waiting for halted")
    false
  }

  /** Poll dmstatus until allrunning=1.  Returns false on timeout. */
  def waitRunning(timeout: Int = 5000): Boolean = {
    var t = timeout
    while (t > 0) {
      val st = read(0x11)
      if (((st >> 11) & 1) == 1) return true // allrunning bit 11
      t -= 1
    }
    println(s"[DMI] TIMEOUT waiting for running")
    false
  }

  /** Halt the core. */
  def halt(): Boolean = {
    write(0x10, BigInt("80000001", 16))  // dmcontrol: haltreq=1, dmactive=1
    val ok = waitHalted()
    write(0x10, BigInt("00000001", 16))  // deassert haltreq
    ok
  }

  /** Resume the core. */
  def resume(): Boolean = {
    write(0x10, BigInt("40000001", 16))  // dmcontrol: resumereq=1, dmactive=1
    cd.waitSampling(4)
    write(0x10, BigInt("00000001", 16))  // deassert resumereq
    waitRunning()
  }

  /** Write a 32-bit value to a GPR via abstract command. */
  def regWrite(regno: Int, data: BigInt): Boolean = {
    write(0x04, data)          // data0 = value
    // command: cmdtype=0, aarsize=2, postexec=0, transfer=1, write=1
    write(0x17, BigInt(0x00230000 | regno))
    waitNotBusy()
  }

  /** Read a 32-bit value from a GPR via abstract command. */
  def regRead(regno: Int): BigInt = {
    // command: cmdtype=0, aarsize=2, postexec=0, transfer=1, write=0
    write(0x17, BigInt(0x00220000 | regno))
    if (!waitNotBusy()) return BigInt(-1)
    read(0x04)                 // data0
  }

  /**
   * Execute a 2-instruction progbuf snippet: instr0; ebreak .
   * Returns true on success (busy=0, cmderr=0).
   */
  def execProgbuf(instr0: BigInt, instr1: BigInt = BigInt("00100073", 16)): Boolean = {
    write(0x20, instr0)                          // progbuf[0]
    write(0x21, instr1)                          // progbuf[1] = ebreak
    write(0x22, BigInt("00100073", 16))          // progbuf[2] = ebreak (safety)
    write(0x23, BigInt("00100073", 16))          // progbuf[3] = ebreak (safety)
    // command: cmdtype=0, aarsize=2, postexec=1, transfer=0, write=0, regno=0
    // 0x00240000 = (aarsize=2 << 20) | (postexec=1 << 18)
    write(0x17, BigInt("00240000", 16))
    waitNotBusy()
  }

  /**
   * Write a 32-bit word to memory via progbuf:
   *   progbuf[] = { sw s0, 0(s1); ebreak }
   * x9/s1 ← addr, x8/s0 ← data, then execute.
   */
  def memWrite(addr: BigInt, data: BigInt): Boolean = {
    if (!regWrite(0x1009, addr)) return false  // x9 = s1 = addr
    if (!regWrite(0x1008, data)) return false  // x8 = s0 = data
    // sw s0, 0(s1): 0x0084a023
    execProgbuf(BigInt("0084a023", 16))
  }

  /**
   * Read a 32-bit word from memory via progbuf:
   *   progbuf[] = { lw s0, 0(s1); ebreak }
   * x9/s1 ← addr, then execute and read x8.
   * Returns BigInt(-1) on error.
   */
  def memRead(addr: BigInt): BigInt = {
    if (!regWrite(0x1009, addr)) return BigInt(-1)
    // lw s0, 0(s1): 0x0004a403
    if (!execProgbuf(BigInt("0004a403", 16))) return BigInt(-1)
    regRead(0x1008)
  }
}


// =============================================================================
// RVDebugFpgaSim
//
// Stand-alone simulation that drives the full RVDebugSimTop SoC and replays
// the OpenOCD command sequence via DMI:
//   1.  Let the core boot / run a few hundred cycles
//   2.  Halt via DMI
//   3.  Verify known register values (a0=0x12345678…)
//   4.  fence.i execution test (OpenOCD probe)
//   5.  Memory write via progbuf  (mww 0x80001000 0xCAFEF00D)
//   6.  Memory read  via progbuf  (mdw 0x80001000 — verify round-trip)
//   7.  Write 0xDEAD1234 to a0 via abstract command
//   8.  Resume
//
// Run with:   sbt "runMain rv.RVDebugFpgaSim"
// Waveform:   simWorkspace/RVDebugSimTop/wave.fst
// =============================================================================
object RVDebugFpgaSim extends App {

  // ---- Build firmware -------------------------------------------------------
  val CC      = "riscv32-unknown-elf-gcc"
  val OBJCOPY = "riscv32-unknown-elf-objcopy"
  val INCDIR  = "test/sim/common"
  val LD      = "test/sim/common/memmap_rv.ld"
  val NAME    = "debug_fpga"

  import java.io.File; import java.nio.file.{Files, Paths}; import sys.process._
  val tmpDir  = s"test/sim/$NAME/tmp"
  new File(tmpDir).mkdirs()

  val srcList = "test/sim/debug_fpga/src/main.S"
  val elf     = s"$tmpDir/$NAME.elf"
  val bin     = s"$tmpDir/$NAME.bin"
  val cc_flags = "-march=rv32i -mabi=ilp32 -Os -ffreestanding -nostdlib -nostartfiles -Wl,--no-warn-rwx-segments"

  require(s"$CC $cc_flags -I $INCDIR $srcList -T $LD -o $elf".! == 0, "Firmware compilation failed")
  require(s"$OBJCOPY -O binary $elf $bin".! == 0, "objcopy failed")

  val bytes  = Files.readAllBytes(Paths.get(bin))
  val nWords = (bytes.length + 3) / 4
  val program = new Array[BigInt](nWords)
  for (i <- 0 until nWords) {
    var w = BigInt(0)
    for (b <- 0 until 4) {
      val idx = i * 4 + b
      w |= BigInt(if (idx < bytes.length) bytes(idx).toInt & 0xFF else 0) << (b * 8)
    }
    program(i) = w
  }
  println(s"[$NAME] loaded $nWords words from $bin")

  // ---- Simulation -----------------------------------------------------------
  val simConf = RVConfig(
    supportFormal    = false,
    supportMulDiv    = false,
    supportDebug     = true,
    useXilinxJtag    = false,   // irrelevant: no DTM in RVDebugSimTop
    InstAddrSize     = 32,
    dataAddrSize     = 32,
    bootVector       = BigInt("80000040", 16))

  SimConfig
    .withFstWave
    .workspaceName("RVDebugSimTop")
    .compile(new RVDebugSimTop(simConf))
    .doSim { dut =>
      // ---- Load program into memory ----------------------------------------
      val memOffset = 16   // 0x80000040 → word offset 16 in the 16 KiB SRAM
      val memBank0  = dut.core.instrMem.mem.u_mem_bank0.u_mem
      val memBank1  = dut.core.instrMem.mem.u_mem_bank1.u_mem
      for (i <- 0 until 4096) { memBank0.setBigInt(i, 0); memBank1.setBigInt(i, 0) }
      for (i <- program.indices) {
        memBank0.setBigInt(memOffset + i, program(i))
        memBank1.setBigInt(memOffset + i, program(i))
      }
      for (j <- 0 until 32) dut.core.rv.RegFile.RegMem.setBigInt(j, 0)

      // ---- Clock + reset ---------------------------------------------------
      dut.clockDomain.forkStimulus(10)
      dut.io.irq        #= false
      dut.io.timer_irq #= false
      dut.io.uart.rxd  #= true   // UART idle high

      val dmi = new DmiDriver(dut.io.dmi, dut.clockDomain)

      // ---- 1. Activate debug module (dmactive=1) ---------------------------
      dmi.write(0x10, 1)           // dmcontrol: dmactive=1
      dut.clockDomain.waitSampling(200)

      // ---- 2. Halt ---------------------------------------------------------
      println(s"[$NAME] Halting core...")
      assert(dmi.halt(), s"[$NAME] FAIL: halt timed out")
      println(s"[$NAME] Core halted. dmstatus=0x${dmi.read(0x11).toString(16)}")

      // ---- 3. Verify register values ---------------------------------------
      val a0 = dmi.regRead(0x100A)  // x10 = a0
      val a1 = dmi.regRead(0x100B)  // x11 = a1
      println(f"[$NAME] a0=0x${a0}%08X (expect 0x12345678)")
      println(f"[$NAME] a1=0x${a1}%08X (expect 0xDEAD0000)")
      assert(a0 == BigInt("12345678", 16), s"[$NAME] FAIL: a0 mismatch (got 0x${a0.toString(16)})")
      assert(a1 == BigInt("DEAD0000", 16), s"[$NAME] FAIL: a1 mismatch (got 0x${a1.toString(16)})")

      // ---- 4. fence.i test (mimics OpenOCD progbuf startup probe) ----------
      println(s"[$NAME] Testing fence.i via progbuf...")
      val fenceOk = dmi.execProgbuf(BigInt("0000100f", 16))  // fence.i
      println(f"[$NAME] fence.i result: ${if (fenceOk) "OK" else "FAIL"}")
      assert(fenceOk, s"[$NAME] FAIL: fence.i via progbuf failed (abstractcs=0x${dmi.read(0x16).toString(16)})")

      // ---- 5. Memory write via progbuf ------------------------------------
      val testAddr = BigInt("80001000", 16)
      val testData = BigInt("CAFEF00D", 16)
      println(f"[$NAME] Writing 0x${testData}%08X to 0x${testAddr}%08X via progbuf...")
      val writeOk = dmi.memWrite(testAddr, testData)
      assert(writeOk, s"[$NAME] FAIL: memWrite timed out (abstractcs=0x${dmi.read(0x16).toString(16)})")
      println(s"[$NAME] memWrite OK")

      // ---- 6. Memory read via progbuf --------------------------------------
      println(f"[$NAME] Reading from 0x${testAddr}%08X via progbuf...")
      val readBack = dmi.memRead(testAddr)
      println(f"[$NAME] readBack=0x${readBack}%08X (expect 0x${testData}%08X)")
      assert(readBack == testData,
        f"[$NAME] FAIL: memRead mismatch (got 0x${readBack}%08X, expected 0x${testData}%08X)")
      println(s"[$NAME] memRead OK - round-trip PASSED")

      // ---- 7. Write a new value into a0 via abstract command ---------------
      val newA0 = BigInt("DEAD1234", 16)
      dmi.regWrite(0x100A, newA0)
      val readA0 = dmi.regRead(0x100A)
      println(f"[$NAME] After write: a0=0x${readA0}%08X (expect 0x${newA0}%08X)")
      assert(readA0 == newA0, f"[$NAME] FAIL: reg write-back mismatch")

      // ---- 8. Resume -------------------------------------------------------
      println(s"[$NAME] Resuming core...")
      assert(dmi.resume(), s"[$NAME] FAIL: resume timed out")
      println(s"[$NAME] Core running. dmstatus=0x${dmi.read(0x11).toString(16)}")

      dut.clockDomain.waitSampling(200)

      println(s"[$NAME] *** ALL TESTS PASSED ***")
    }
}
