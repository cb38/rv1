package rv
import spinal.core._
import spinal.lib._ 
import spinal.lib.bus.amba4.axilite._
import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.misc.pipeline._
import scala.io.Source
import multiport_memory._




////////////////////////////////////////////////
object Assembler {

  val prog = Array[Int]()

  // collect destination addresses in first pass
  val symbols = collection.mutable.Map[String, Int]()

  def getProgram(prog: String) = assemble(prog)

  def assemble(prog: String): Array[Int] = {
    assemble(prog, false)
    assemble(prog, true)
  }

  def assemble(prog: String, pass2: Boolean): Array[Int] = {

    val source = Source.fromFile(prog)
    var program = List[Int]()
    var pc = 0

    def toInt(s: String): Int = {
      if (s.startsWith("0x")) {
        Integer.parseInt(s.substring(2), 16)
      } else {
        Integer.parseInt(s)
      }
    }

    def regNumber(s: String): Int = {
      assert(s.startsWith("r"))
      s.substring(1).toInt
    }
    for (line <- source.getLines()) {
      
      val tokens = line.trim.split(" ")
      val Pattern = "(.*:)".r
      val instruction = tokens(0) match {
        case "#" => // comment
        case Pattern(l) => if (!pass2) symbols += (l.substring(0, l.length - 1) -> pc)
        case "add"    =>   regNumber(tokens(3)) << 20 | regNumber(tokens(2)) << 15 | regNumber(tokens(1)) << 7 | 0x33
        case "sub"    =>   0x20 << 25 | regNumber(tokens(3)) << 20 | regNumber(tokens(2)) << 15 | regNumber(tokens(1)) << 7 | 0x33     
        case "addi"   =>   toInt(tokens(3)) << 20 | regNumber(tokens(2)) << 15 | regNumber(tokens(1)) << 7 | 0x13
        case "lui"    =>   toInt(tokens(2)) << 12 | regNumber(tokens(1)) << 7  | 0x37
        case "jal"    =>   toInt(tokens(2)) << 20 | regNumber(tokens(1)) << 7  | 0x6f
        case "lw"     =>   regNumber(tokens(3)) << 15 | toInt(tokens(2)) << 20 | 0x2 <<12 | regNumber(tokens(1)) << 7 | 0x03
        case "sw"     =>   regNumber(tokens(3)) << 15 | toInt(tokens(2)) << 7  | 0x2 <<12 | regNumber(tokens(1)) << 20 | 0x23
        case "lhu"    =>   regNumber(tokens(3)) << 15 | toInt(tokens(2)) << 20  | 0x5 <<12 | regNumber(tokens(1)) << 7 | 0x03
        case "lb"     =>   regNumber(tokens(3)) << 15 | toInt(tokens(2)) << 20  | 0x0 <<12 | regNumber(tokens(1)) << 7 | 0x03
        case "lbu"    =>   regNumber(tokens(3)) << 15 | toInt(tokens(2)) << 20  | 0x4 <<12 | regNumber(tokens(1)) << 7 | 0x03
        case "sb"     =>   regNumber(tokens(3)) << 15 | toInt(tokens(2)) << 7  | 0x0 <<12 | regNumber(tokens(1)) << 20 | 0x23
        case "" => // println("Empty line")
        case t: String => throw new Exception("Assembler error: unknown instruction")
        case _ => throw new Exception("Assembler error")
      }
      // print in hex
      if (!pass2) println(line, (instruction match {
        case (a: Int) => f"0x$a%08x"
        case _ => "----"
      }))
     

      instruction match {
        
        case (a: Int) => {
          
            program = a :: program
            pc += 1
          
        }
        case _ => // println("Something else")
      }
    }
    val finalProg = program.reverse.toArray
    if (!pass2) {
      println(s"The program:")
      finalProg.foreach(printf("0x%04x ", _))
      println()
    }
    finalProg
  }
}


object RVVerilog extends App {
  val config=SpinalConfig(device=Device.XILINX,targetDirectory = "hw/gen",mergeAsyncProcess = true)
 
  config.generateVerilog(new RV(config = RVConfig(supportFormal = false,
                                                 supportMul = false,
                                                 supportDiv = true,
                                                 supportCsr = true,
                                                 bootVector = BigInt("80000040", 16)))).printPruned()
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
                addressWidth = config.pcSize,
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
        rv.io.irq       := io.irq
        rv.io.timer_irq := io.timer_irq

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
                                                 supportMul = false,
                                                 supportDiv = false,
                                                 supportCsr = false,
                                                 pcSize = 16,
                                                 dataAddrSize = 16,
                                                 bootVector = BigInt("00000000", 16))))
}

object RVSim extends App {
  
     val program = Assembler.assemble("asm/RV.asm")
   
     


   SimConfig.withFstWave.compile(new RVTop(config = RVConfig(supportFormal = true,
                                                 supportMul = false,
                                                 supportDiv = false,
                                                 supportCsr = false,
                                                 pcSize = 32,
                                                 dataAddrSize = 32,
                                                 bootVector = BigInt("80000000", 16)) )).doSim(seed = 2){ dut =>
  val instrMem = dut.core.instrMem.mem.u_mem
  val dataMem  = dut.core.dataMem.mem.u_mem
   for (i <- 0 until program.length) {
            instrMem.setBigInt(i, program(i))
            dataMem.setBigInt(i, program(i))
        }
  // fill the rest of memory with 0
  for (i <- program.length until 1024) {
            instrMem.setBigInt(i, 0)
            dataMem.setBigInt(i, 0)
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
   while(run>0 && (dut.core.rv.exec.instr.toBigInt != 0 ||  (dut.core.rv.decoder.pc.toBigInt & 0xFFFFFF) < 16)) {
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
    for (j <- 14 until 32 by 2) {
        printf("mem[%02d]:  %08X   mem[%02d]:  %08X ", j,dataMem.getBigInt(j),j+1,dataMem.getBigInt(j+1))
        println()  
    }
        // check r1 is 0 and r4 is the address of the jal then test passed
    if(dut.core.rv.RegFile.RegMem.getBigInt(1) == 0 && (dut.core.rv.RegFile.RegMem.getBigInt(4) & 0xFFFFF) == 0x34){
        println("Test passed!")
    } else {
        println("Test failed!") 
    }   
    
 }
}