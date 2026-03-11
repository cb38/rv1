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

// top soc exmaple to test the RV core with AXI4-Lite memory and GPIO
class RVTop(config: RVConfig) extends Component {
    val io = new Bundle {
        //val osc_clk = in(Bool)

        val led1    = out(Bool)
        val led2    = out(Bool)
        val led3    = out(Bool)

        val button = in(Bool)
        val irq    = in(Bool)
        val timer_irq = in(Bool)
    }

    
    

    val core = new Area {

        val rv = new RV(config)
        val cfg = MemConfig(memorySize = 64*1024, dataWidth = 32) //TODO: make it configurable

        val instrMem = new AXI4LiteReadOnly_Mem(
            AxiLite4Config(
                addressWidth = config.InstAddrSize,
                dataWidth    = 32
            ),
            cfg
        )

        val dataMem = new AXI4Lite_Mem(
            AxiLite4Config(
                addressWidth = config.dataAddrSize,
                dataWidth    = 32
            ),
            cfg
        )
        rv.io.instr_axi <> instrMem.io.axi
        rv.io.data_axi  <> dataMem.io.axi

        // CLINT on periph_axi port (address decode done inside CPU)
        val clint = new CLINT(config.dataAddrSize)
        rv.io.periph_axi <> clint.io.bus

        rv.io.irq       := io.irq
        rv.io.timer_irq := clint.io.timerIrq || io.timer_irq
        rv.io.soft_irq  := clint.io.softIrq

        val awSeen = RegInit(False)
        val wSeen  = RegInit(False)
        val awAddr = Reg(UInt(config.dataAddrSize bits)) init(0)
        val wData  = Reg(Bits(32 bits)) init(0)

        when(rv.io.data_axi.aw.fire) {
            awSeen := True
            awAddr := rv.io.data_axi.aw.addr
        }
        when(rv.io.data_axi.w.fire) {
            wSeen := True
            wData := rv.io.data_axi.w.data
        }

        val writeCommit = awSeen && wSeen
        when(writeCommit) {
            awSeen := False
            wSeen  := False
        }

        val update_leds = writeCommit && awAddr(9)
        io.led1 := RegNextWhen(wData(0), update_leds) init(False)
        io.led2 := RegNextWhen(wData(1), update_leds) init(False)
        io.led3 := RegNextWhen(wData(2), update_leds) init(False)
      }

     

    
}


object RVTopVerilog extends App {
  val config=SpinalConfig(device=Device.XILINX,targetDirectory = "hw/gen",mergeAsyncProcess = true)
 
  config.generateVerilog(new RVTop(config = RVConfig(supportFormal = false,
                                                 supportMulDiv = false,
                                                 InstAddrSize = 16,
                                                 dataAddrSize = 16,
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