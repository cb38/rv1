package rv
import spinal.core._
import spinal.lib._ 
import spinal.lib.bus.amba4.axilite._
import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.misc.pipeline._
import scala.io.Source
import scala.sys.process._
import java.io.File
import java.nio.file.{Files, Paths}
import multiport_memory._
import spinal.lib.bus.amba4.axi._
import spinal.lib.com.jtag.Jtag
import spinal.lib.com.uart.{Apb3UartCtrl, Uart, UartCtrlGenerics, UartCtrlMemoryMappedConfig}
import spinal.lib.bus.amba3.apb._


/** Minimal AXI4Lite -> APB3 bridge.
  * axiAddrWidth : width of the AXI address bus (e.g. 32)
  * apbAddrWidth : width of the APB3 PADDR (e.g. 5 for Apb3UartCtrl)
  * One read or write in flight at a time (AXI4Lite rule). */
class Axi4ToApb3Bridge(axiAddrWidth: Int, apbAddrWidth: Int, dataWidth: Int) extends Component {
    val apbCfg = Apb3Config(addressWidth = apbAddrWidth, dataWidth = dataWidth, useSlaveError = false)
    val io = new Bundle {
        val axi = slave (AxiLite4(AxiLite4Config(axiAddrWidth, dataWidth)))
        val apb = master(Apb3(apbCfg))
    }

    // ---- Shared state ----
    val addr   = Reg(UInt(axiAddrWidth bits)) init(0)
    val wdata  = Reg(Bits(dataWidth bits))    init(0)
    val isRead = RegInit(False)

    // ---- AXI4Lite default handshake signals ----
    io.axi.ar.ready := False
    io.axi.aw.ready := False
    io.axi.w.ready  := False
    io.axi.r.valid  := False
    io.axi.r.data   := 0
    io.axi.r.resp   := B"00"
    io.axi.b.valid  := False
    io.axi.b.resp   := B"00"

    // ---- APB3 defaults ----
    io.apb.PADDR   := 0
    io.apb.PSEL    := B"0"
    io.apb.PENABLE := False
    io.apb.PWRITE  := False
    io.apb.PWDATA  := 0

    val state = RegInit(U(0, 2 bits))  // 0=Idle, 1=Setup, 2=Access, 3=Done

    switch(state) {
        is(0) {  // Idle
            when(io.axi.ar.valid) {
                addr   := io.axi.ar.addr
                isRead := True
                io.axi.ar.ready := True
                state := 1
            } elsewhen(io.axi.aw.valid && io.axi.w.valid) {
                addr   := io.axi.aw.addr
                wdata  := io.axi.w.data
                isRead := False
                io.axi.aw.ready := True
                io.axi.w.ready  := True
                state := 1
            }
        }
        is(1) {  // Setup: PSEL=1, PENABLE=0
            io.apb.PADDR   := addr(apbAddrWidth - 1 downto 0)
            io.apb.PSEL    := B"1"
            io.apb.PENABLE := False
            io.apb.PWRITE  := !isRead
            io.apb.PWDATA  := wdata
            state := 2
        }
        is(2) {  // Access: PSEL=1, PENABLE=1, wait PREADY
            io.apb.PADDR   := addr(apbAddrWidth - 1 downto 0)
            io.apb.PSEL    := B"1"
            io.apb.PENABLE := True
            io.apb.PWRITE  := !isRead
            io.apb.PWDATA  := wdata
            when(io.apb.PREADY) { state := 3 }
        }
        is(3) {  // Done: return response to AXI master
            when(isRead) {
                io.axi.r.valid := True
                io.axi.r.data  := io.apb.PRDATA
                io.axi.r.resp  := B"00"
                when(io.axi.r.ready) { state := 0 }
            } otherwise {
                io.axi.b.valid := True
                io.axi.b.resp  := B"00"
                when(io.axi.b.ready) { state := 0 }
            }
        }
    }
}
 

/** AXI4Lite 1-to-N address decoder.
  * addrMap: sequence of (baseAddress, size) one entry per slave output.
  *   address match: (addr & ~(size-1)) == base   (base and size must be power-of-2 aligned)
  * Allows one outstanding read and one outstanding write at a time (AXI4Lite rule). */
class AxiLite4Decoder(axiConfig: AxiLite4Config,
                      addrMap  : Seq[(BigInt, BigInt)]) extends Component {
    val n = addrMap.length
    val io = new Bundle {
        val input   = slave  (AxiLite4(axiConfig))
        val outputs = Vec(master(AxiLite4(axiConfig)), n)
    }

    // Combinatorial: one-hot slave select from address
    def slaveOf(addr: UInt): Bits = {
        val addrMask = (BigInt(1) << axiConfig.addressWidth) - 1  // keep only addressWidth bits
        val sel = Bits(n bits)
        sel := 0
        for (i <- 0 until n) {
            val (base, size) = addrMap(i)
            val mask = (~(size - 1)) & addrMask   // e.g. size=0x1000,w=16 → 0xF000
            when ((addr & U(mask, axiConfig.addressWidth bits)) ===
                   U(base, axiConfig.addressWidth bits)) { sel(i) := True }
        }
        sel
    }

    // ---- Read path ----
    val rSel     = Reg(Bits(n bits)) init(0)
    val rPending = RegInit(False)

    io.input.ar.ready := !rPending
    for (i <- 0 until n) {
        io.outputs(i).ar.valid := io.input.ar.valid && slaveOf(io.input.ar.addr)(i) && !rPending
        io.outputs(i).ar.addr  := io.input.ar.addr
        io.outputs(i).ar.prot  := io.input.ar.prot
    }
    when (io.input.ar.fire) { rSel := slaveOf(io.input.ar.addr); rPending := True }

    io.input.r.valid := False
    io.input.r.data  := 0
    io.input.r.resp  := 0
    for (i <- 0 until n) {
        io.outputs(i).r.ready := rSel(i) && io.input.r.ready
        when (rSel(i)) {
            io.input.r.valid := io.outputs(i).r.valid
            io.input.r.data  := io.outputs(i).r.data
            io.input.r.resp  := io.outputs(i).r.resp
        }
    }
    when (io.input.r.fire) { rPending := False }

    // ---- Write path ----
    val wSel     = Reg(Bits(n bits)) init(0)
    val wPending = RegInit(False)

    io.input.aw.ready := !wPending
    for (i <- 0 until n) {
        io.outputs(i).aw.valid := io.input.aw.valid && slaveOf(io.input.aw.addr)(i) && !wPending
        io.outputs(i).aw.addr  := io.input.aw.addr
        io.outputs(i).aw.prot  := io.input.aw.prot
    }
    when (io.input.aw.fire) { wSel := slaveOf(io.input.aw.addr); wPending := True }

    io.input.w.ready := False
    for (i <- 0 until n) {
        io.outputs(i).w.valid := io.input.w.valid && wSel(i) && wPending
        io.outputs(i).w.data  := io.input.w.data
        io.outputs(i).w.strb  := io.input.w.strb
        when (wSel(i) && wPending) { io.input.w.ready := io.outputs(i).w.ready }
    }

    io.input.b.valid := False
    io.input.b.resp  := 0
    for (i <- 0 until n) {
        io.outputs(i).b.ready := wSel(i) && io.input.b.ready
        when (wSel(i)) {
            io.input.b.valid := io.outputs(i).b.valid
            io.input.b.resp  := io.outputs(i).b.resp
        }
    }
    when (io.input.b.fire) { wPending := False }
}



object RVVerilog extends App {
  val config=SpinalConfig(device=Device.XILINX,targetDirectory = "hw/gen",mergeAsyncProcess = true)
 
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
  val config=SpinalConfig(device=Device.XILINX,targetDirectory = "hw/gen",mergeAsyncProcess = true)
 
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
// Peripheral address map (CPU periph_axi port, addr[31:16] == 0x0200):
//   0x0200_0000 .. 0x0200_0FFF  CLINT  (4 KiB)
//   0x0200_1000 .. 0x0200_1FFF  UART   (4 KiB, via AXI4Lite->APB3 bridge)
class RVTop(config: RVConfig) extends Component {
    val io = new Bundle {
        val button    = in(Bool)
        val irq       = in(Bool)
        val timer_irq = in(Bool)
        val uart      = master(Uart())
        // JTAG debug port (only present when supportDebug = true)
        val jtag_tck    = if(config.hasDebug) in(Bool)  else null
        val jtag_tms    = if(config.hasDebug) in(Bool)  else null
        val jtag_tdi    = if(config.hasDebug) in(Bool)  else null
        val jtag_tdo    = if(config.hasDebug) out(Bool) else null
        val jtag_trst_n = if(config.hasDebug) in(Bool)  else null
    }

    val core = new Area {

        val rv = new RV(config)
        val cfg = MemConfig(memorySize = 64*1024, dataWidth = 32)

        val instrMem = new AXI4LiteReadOnly_Mem(
            AxiLite4Config(addressWidth = config.InstAddrSize, dataWidth = 32), cfg)
        val dataMem  = new AXI4Lite_Mem(
            AxiLite4Config(addressWidth = config.dataAddrSize, dataWidth = 32), cfg)

        rv.io.instr_axi <> instrMem.io.axi
        rv.io.data_axi  <> dataMem.io.axi

        // -----------------------------------------------------------------------
        // Peripheral crossbar: periph_axi -> AxiLite4Decoder -> slaves
        //   outputs(0) -> CLINT       [0x0000..0x0FFF]
        //   outputs(1) -> APB bridge  [0x1000..0x1FFF] -> UART
        // -----------------------------------------------------------------------
        val clint = new CLINT(config.dataAddrSize)

        val apbBridge = new Axi4ToApb3Bridge(
            axiAddrWidth = config.dataAddrSize,
            apbAddrWidth = 5,   // Apb3UartCtrl uses 5-bit PADDR
            dataWidth    = 32
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
        uartCtrl.io.uart <> io.uart
        apbBridge.io.apb <> uartCtrl.io.apb

        val periphXbar = new AxiLite4Decoder(
            axiConfig = AxiLite4Config(addressWidth = config.dataAddrSize, dataWidth = 32),
            addrMap   = Seq(
                (BigInt(0x0000), BigInt(0x1000)),  // outputs(0): CLINT
                (BigInt(0x1000), BigInt(0x1000))   // outputs(1): APB bridge -> UART
            )
        )
        rv.io.periph_axi         <> periphXbar.io.input
        periphXbar.io.outputs(0) <> clint.io.bus
        periphXbar.io.outputs(1) <> apbBridge.io.axi

        rv.io.irq       := io.irq
        rv.io.timer_irq := clint.io.timerIrq || io.timer_irq
        rv.io.soft_irq  := clint.io.softIrq

        // -----------------------------------------------------------------------
        // Debug: JtagDTM -> DebugModule -> rv.io.debug
        // -----------------------------------------------------------------------
        if (config.hasDebug) {
            val dtm   = new JtagDTM()
            val dbgMod = new DebugModule()

            dtm.io.tck    := io.jtag_tck
            dtm.io.tms    := io.jtag_tms
            dtm.io.tdi    := io.jtag_tdi
            io.jtag_tdo   := dtm.io.tdo
            dtm.io.trst_n := io.jtag_trst_n

            dtm.io.dmi <> dbgMod.io.dmi

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
  val config=SpinalConfig(device=Device.XILINX,targetDirectory = "hw/gen",mergeAsyncProcess = true)
 
  config.generateVerilog(new RVTop(config = RVConfig(supportFormal = false,
                                                 supportMulDiv = false,
                                                 InstAddrSize = 32,
                                                 dataAddrSize = 32,
                                                 supportZbs = true,
                                                 supportZba = true,
                                                 supportZbb = true,
                                                 supportZbkb = true,
                                                 supportDebug = true,
                                                 bootVector = BigInt("00000000", 16))))
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
   

   SimConfig.withFstWave.compile(new RVTop(config = RVConfig(supportFormal = true,
                                                 supportMulDiv = false,
                                                 InstAddrSize = 32,
                                                 dataAddrSize = 32,
                                                 bootVector = BigInt("80000040", 16)) )).doSim{ dut =>
  val instrMem = dut.core.instrMem.mem.u_mem
  val dataMem  = dut.core.dataMem.mem.u_mem
  // Binary starts at _start (0x80000040), which maps to word offset 16 in memory
  val memOffset = 16
  // Clear all memory first
  for (i <- 0 until 1024) {
            instrMem.setBigInt(i, 0)
            dataMem.setBigInt(i, 0)
        }
  // Load program at correct offset
  for (i <- 0 until program.length) {
            instrMem.setBigInt(memOffset + i, program(i))
            dataMem.setBigInt(memOffset + i, program(i))
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
        printf("mem[%02d]:  %08X   mem[%02d]:  %08X ", j,dataMem.getBigInt(j),j+1,dataMem.getBigInt(j+1))
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