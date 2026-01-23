package rv
import spinal.core._
import spinal.lib._ 
import spinal.lib.fsm._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.misc.pipeline._
import scala.io.Source
import multiport_memory._


object Opcodes extends SpinalEnum {
    def LUI         = B"0110111"
    def AUIPC       = B"0010111"
    def JAL         = B"1101111"
    def JALR        = B"1100111"
    def B           = B"1100011"
    def L           = B"0000011"
    def S           = B"0100011"
    def ALUI        = B"0010011"
    def ALU         = B"0110011"
    def F           = B"0001111"
    def SYS         = B"1110011"
}
object InstrFormat extends SpinalEnum(binaryOneHot) {
    val R       = newElement()
    val I       = newElement()
    val S       = newElement()
    val B       = newElement()
    val U       = newElement()
    val J       = newElement()
    val Shamt   = newElement()
    val CSR     = newElement()
}

object InstrType extends SpinalEnum(binaryOneHot) {
    val Undef   = newElement()
    val JAL     = newElement()
    val JALR    = newElement()
    val B       = newElement()
    val L       = newElement()
    val S       = newElement()
    val ALU_ADD = newElement()
    val ALU     = newElement()
    val SHIFT   = newElement()
    val FENCE   = newElement()
    val E       = newElement()
    val CSR     = newElement()
    val MULDIV  = newElement()
}

object CsrCmd extends SpinalEnum(binaryOneHot) {
    val NONE    = newElement()
    val WRITE   = newElement()
    val SET     = newElement()
    val CLEAR   = newElement()
}

object MulOp extends SpinalEnum(binaryOneHot) {
    val NONE    = newElement()
    val MUL     = newElement()
    val MULH    = newElement()
    val MULHSU  = newElement()
    val MULHU   = newElement()
}

case class RVFI() extends Bundle {

    val valid       = Bool
    val order       = UInt(64 bits)
    val insn        = Bits(32 bits)
    val trap        = Bool
    val halt        = Bool
    val intr        = Bool
    val rs1_addr    = UInt(5 bits)
    val rs2_addr    = UInt(5 bits)
    val rs1_rdata   = Bits(32 bits)
    val rs2_rdata   = Bits(32 bits)
    val rd_addr     = UInt(5 bits)
    val rd_wdata    = Bits(32 bits)
    val pc_rdata    = UInt(32 bits)
    val pc_wdata    = UInt(32 bits)
    val mem_addr    = UInt(32 bits)
    val mem_rmask   = Bits(4 bits)
    val mem_wmask   = Bits(4 bits)
    val mem_rdata   = Bits(32 bits)
    val mem_wdata   = Bits(32 bits)
    val csr_mstatus_wmask = Bits(32 bits)
    val csr_mstatus_wdata = Bits(32 bits)
    val csr_mepc_wmask    = Bits(32 bits)
    val csr_mepc_wdata    = Bits(32 bits)
    val csr_mcause_wmask  = Bits(32 bits)
    val csr_mcause_wdata  = Bits(32 bits)
    val ixl         = Bits(2 bits)
    val mode        = Bits(2 bits)

    def init() : RVFI = {
        valid     init(False) addAttribute("keep")
        order     init(0) addAttribute("keep")
        insn      init(0) addAttribute("keep")
        trap      init(False) addAttribute("keep")
        halt      init(False) addAttribute("keep")
        intr      init(False) addAttribute("keep")
        rs1_addr  init(0) addAttribute("keep")
        rs2_addr  init(0) addAttribute("keep")
        rd_addr   init(0) addAttribute("keep")
        rs1_rdata init(0) addAttribute("keep")
        rs2_rdata init(0) addAttribute("keep")
        rd_wdata  init(0) addAttribute("keep")
        pc_rdata  init(0) addAttribute("keep")
        pc_wdata  init(0) addAttribute("keep")
        mem_addr  init(0) addAttribute("keep")
        mem_rmask init(0) addAttribute("keep")
        mem_rdata init(0) addAttribute("keep")
        mem_wmask init(0) addAttribute("keep")
        mem_wdata init(0) addAttribute("keep")
        csr_mstatus_wmask init(0) addAttribute("keep")
        csr_mstatus_wdata init(0) addAttribute("keep")
        csr_mepc_wmask    init(0) addAttribute("keep")
        csr_mepc_wdata    init(0) addAttribute("keep")
        csr_mcause_wmask  init(0) addAttribute("keep")
        csr_mcause_wdata  init(0) addAttribute("keep")
        ixl       init(1) addAttribute("keep")
        mode      init(3) addAttribute("keep")

        this
    }
}


case class RVConfig(
                supportMul      : Boolean = false,
                supportDiv      : Boolean = false,
                supportCsr      : Boolean = false,
                supportFormal   : Boolean = false,
                supportFence    : Boolean = false,
                pcSize          : Int     = 32,
                dataAddrSize    : Int     = 32,
                reflopDataRsp   : Boolean = true
                ) {

    def hasMul      = supportMul
    def hasDiv      = supportDiv
    def hasCsr      = supportCsr
    def hasFence    = supportFence


    def hasFormal   = supportFormal
}

case class InstrReqIntfc(config: RVConfig) extends Bundle() {

        val valid       = out(Bool)
        val ready       = in(Bool)
        val addr        = out(UInt(config.pcSize bits))
}

case class InstrRspIntfc(config: RVConfig) extends Bundle() {

        val valid       = in(Bool)
        val data        = in(Bits(32 bits))
}

case class DataReqIntfc(config: RVConfig) extends Bundle() {

        val valid       = out(Bool)
        val ready       = in(Bool)
        
        val rden        = out(Bool)
        val rdaddr      = out(UInt(config.dataAddrSize bits))
        val wraddr      = out(UInt(config.dataAddrSize bits))
        val wren        = out(Bool)
        
        val size        = out(Bits(2 bits))
        val data        = out(Bits(32 bits))

}

case class DataRspIntfc(config: RVConfig) extends Bundle() {

        val valid       = in(Bool)
        val data        = in(Bits(32 bits))
}

class RV (config: RVConfig) extends Component {
 
  val fetch, decode, execute = CtrlLink()
  val f2d = StageLink(fetch.down, decode.up)
  val d2e = StageLink(decode.down, execute.up)
  // payload
  val INSTRUCTION = Payload(Bits(32 bits))
  val PC = Payload(Bits(32 bits))
  val rvfi = if (config.hasFormal) Payload(RVFI()) else null
  val rvfiOrder = if (config.hasFormal) Reg(UInt(64 bits)) init(0) else null

  // I/O
  val io = new Bundle {
    val IO = out(UInt(32 bits)).simPublic()
    val rvfi = if (config.hasFormal) out(Reg(RVFI()) init).setName("rvfi") else null
    //val exit = out(Reg(Bool)).init(False)

    val instr_req = InstrReqIntfc(config).setName("instr_req")
    val instr_rsp = InstrRspIntfc(config).setName("instr_rsp")

    val data_req  = DataReqIntfc(config).setName("data_req")
    val data_rsp  = DataRspIntfc(config).setName("data_rsp")
    val irq       = in(Bool).setName("irq")
 }


  // general
  val Iptr = Reg(UInt(32 bits)) init(0)
  val flush = Bool()
  val init = RegInit(False)
    init := True  
  val irqSyncStage0 = RegNext(io.irq).init(False)
  val irqSync       = RegNext(irqSyncStage0).init(False)
  
  //val isData = False
  
  // register file
  val RegFile =  new Area {
   // val rs1_rd      = Bool
    val rs1_rd_addr = UInt(5 bits)
   // val rs2_rd      = Bool
    val rs2_rd_addr = UInt(5 bits)
    val rs1_data    = Bits(32 bits)
    val rs2_data    = Bits(32 bits)
    val rd_wr       = Bool
    val rd_wr_addr  = UInt(5 bits)
    val rd_wr_data  = Bits(32 bits)

    val RegMem = Mem(Bits(32 bits), 32).simPublic()
    val rs1 = Bits(32 bits)
    val rs2 = Bits(32 bits)
    rs1 := RegMem.readAsync(rs1_rd_addr)
    rs2 := RegMem.readAsync(rs2_rd_addr)
    rs1_data := Mux(rs1_rd_addr === 0, B(0, 32 bits), Mux(rs1_rd_addr === rd_wr_addr, rd_wr_data, rs1))
    rs2_data := Mux(rs2_rd_addr === 0, B(0, 32 bits), Mux(rs2_rd_addr === rd_wr_addr, rd_wr_data, rs2))
    RegMem.write(rd_wr_addr, rd_wr_data, rd_wr)
  }

    val CsrRegs = if(config.hasCsr) new Area {
                val mstatus  = Reg(Bits(32 bits)) init(0)
                val mtvec    = Reg(Bits(32 bits)) init(0)
                val mscratch = Reg(Bits(32 bits)) init(0)
                val mepc     = Reg(Bits(32 bits)) init(0)
                val mcause   = Reg(Bits(32 bits)) init(0)
                val mie      = Reg(Bits(32 bits)) init(0)
                val mip      = Reg(Bits(32 bits)) init(0)
        } else null

    if(config.hasCsr){
            CsrRegs.mip(11) := irqSync
    }
  
 
  //mem
  val memory = new Area {
   val isFetch = True
    val readDone = Bool
    val rdData = Bits(32 bits)
    val rdInst = Bits(32 bits)
    
    // data memory
    io.data_req.wren := False
    io.data_req.rden := False
    io.data_req.valid := False
    io.data_req.rdaddr := 0
    io.data_req.wraddr := 0
    io.data_req.size := B"00"
    io.data_req.data := 0
    rdData := io.data_rsp.data
    readDone := io.data_rsp.valid
    when ( io.data_req.valid && !io.data_req.ready)  (execute.haltIt())

    def WriteData(addr: UInt, data: Bits, size : Bits) = {
      io.data_req.wraddr := addr.resized
      io.data_req.data := data
      io.data_req.wren  := True
      io.data_req.size  := size
      io.data_req.valid := True
    }
    def ReadData(addr: UInt) = {
       io.data_req.rdaddr  := addr.resized 
       io.data_req.rden  := True
       io.data_req.valid := True
    }
     // instruction memory 
    val rdAddrI = UInt(config.dataAddrSize bits)
    rdAddrI := 0

    io.instr_req.addr  := rdAddrI.resized 
    io.instr_req.valid := isFetch
    rdInst  := io.instr_rsp.data
    when (!init) (isFetch := False)
    
   }  
  
  val fetcher = new fetch.Area {
    val fifo= new StreamFifo(Bits(32 bits),  2,
        withBypass = true,
        withAsyncRead = true,
        useVec = true
      ) 
    val delayFiring = RegNext (up.isFiring)
    val delayFiring2 = RegNext (delayFiring)
    
    up.valid := RegNext(memory.isFetch).init(False)
    // pc
     when(up.isFiring ) (Iptr := Iptr + 4)
     when (init) (memory.rdAddrI := (Iptr).resized)
     PC := (Iptr-4).asBits 

    // push instruction to fifo   
    fifo.io.push.valid := delayFiring2 
    fifo.io.push.payload := memory.rdInst 
    // pop instruction from fifo
    fifo.io.pop.ready := down.ready
    fifo.io.flush := flush 
    INSTRUCTION := fifo.io.pop.payload // push 4 instructions + offset
    decode.throwWhen(!fifo.io.pop.valid)
   }

  val decoder = new decode.Area {
    
    val instr      = INSTRUCTION.asBits.simPublic()
    val pc         = PC.asBits.simPublic()
    val opcode      = instr(6 downto 0)
    val funct3      = instr(14 downto 12)
    val funct7      = instr(31 downto 25)
    val rd_addr     = U(instr(11 downto 7))
    val rs1_addr    = U(instr(19 downto 15))
    val rs2_addr    = U(instr(24 downto 20))
    val itype       = InstrType()
    val iformat     = InstrFormat()

    val sub         = False
    val unsigned    = False
    iformat         := InstrFormat.R
    itype           := InstrType.Undef

    val csr_addr    = UInt(12 bits)
    val csr_use_imm = False
    val csr_zimm    = UInt(5 bits)
    val csr_cmd     = CsrCmd()
    val csr_supported = False
    val mul_op      = MulOp()

    csr_addr    := instr(31 downto 20).asUInt
    csr_zimm    := U(instr(19 downto 15))
    csr_cmd     := CsrCmd.NONE
    mul_op      := MulOp.NONE

    val valid = Bool().simPublic()
    valid := up.isValid
    object Op1Kind extends SpinalEnum {
        val Rs1     = newElement()
        val Zero    = newElement()
        val Pc      = newElement()
    }

    val op1_kind = Op1Kind()
    op1_kind := Op1Kind.Rs1

    switch(opcode.asBits){
        is(Opcodes.LUI){
            itype               := InstrType.ALU_ADD
            iformat             := InstrFormat.U
            op1_kind            := Op1Kind.Zero
        }
        is(Opcodes.AUIPC){
            itype               := InstrType.ALU_ADD
            iformat             := InstrFormat.U
            op1_kind            := Op1Kind.Pc
        }
        is(Opcodes.JAL){
            itype               := InstrType.JAL
            iformat             := InstrFormat.J
            op1_kind            := Op1Kind.Pc
        }
        is(Opcodes.JALR){
            when(funct3 === B"000") {
                itype           := InstrType.JALR
            }
            iformat             := InstrFormat.I
        }
        is(Opcodes.B){
            when(funct3 =/= B"010" && funct3 =/= B"011") {
                itype           := InstrType.B
            }
            iformat             := InstrFormat.B
            unsigned            := (funct3(2 downto 1) === B"11")
            sub                 := (funct3(2 downto 1) =/= B"00")
        }
        is(Opcodes.L){
            when(funct3 =/= B"011" && funct3 =/= B"110" && funct3 =/= B"111") {
                itype           := InstrType.L
            }
            iformat             := InstrFormat.I
        }
        is(Opcodes.S){
            when(funct3 === B"000" || funct3 === B"001" || funct3 === B"010") {
                itype           := InstrType.S
            }
            iformat             := InstrFormat.S
        }
        is(Opcodes.ALUI){
            switch(funct3.asBits){
                is(B"000"){
                    itype           := InstrType.ALU_ADD
                    iformat         := InstrFormat.I
                }
                is(B"010", B"011") {
                    // ALU_I: SLTI, SLTIU
                    itype           := InstrType.ALU
                    iformat         := InstrFormat.I
                    unsigned        := funct3(0)
                    sub             := True
                }
                is(B"100", B"110", B"111") {
                    // ALU_I: XORI, ORI, ANDI
                    itype           := InstrType.ALU
                    iformat         := InstrFormat.I
                }
                is(B"001"){
                    when(funct7 === B"0000000"){
                        // SHIFT_I: SLLI
                        itype       := InstrType.SHIFT
                    }
                    iformat         := InstrFormat.Shamt
                }
                is(B"101"){
                    when(funct7 === B"0000000" || funct7 === B"0100000"){
                        // SHIFT_I: SRLI, SRAI
                        itype       := InstrType.SHIFT
                    }
                    iformat         := InstrFormat.Shamt
                }
            }
        }
        is(Opcodes.ALU){
            iformat         := InstrFormat.R
            when(funct7 === B"0000001"){
                switch(funct3){
                    is(B"000"){
                        if(config.hasMul){
                            itype   := InstrType.MULDIV
                            mul_op  := MulOp.MUL
                        } else {
                            itype   := InstrType.Undef
                        }
                    }
                    is(B"001"){
                        if(config.hasMul){
                            itype   := InstrType.MULDIV
                            mul_op  := MulOp.MULH
                        } else {
                            itype   := InstrType.Undef
                        }
                    }
                    is(B"010"){
                        if(config.hasMul){
                            itype   := InstrType.MULDIV
                            mul_op  := MulOp.MULHSU
                        } else {
                            itype   := InstrType.Undef
                        }
                    }
                    is(B"011"){
                        if(config.hasMul){
                            itype   := InstrType.MULDIV
                            mul_op  := MulOp.MULHU
                        } else {
                            itype   := InstrType.Undef
                        }
                    }
                    default {
                        itype := InstrType.Undef
                    }
                }
            } otherwise {
                switch(funct7 ## funct3.asBits){
                    is(B"0000000_000", B"0100000_000"){
                        // ADD, SUB
                        itype           := InstrType.ALU_ADD
                        sub             := funct7(5)
                    }
                    is(B"0000000_100", B"0000000_110", B"0000000_111"){
                        // ADD, SUB, XOR, OR, AND
                        itype           := InstrType.ALU
                    }
                    is(B"0000000_001", B"0000000_101", B"0100000_101"){
                        // SLL, SRL, SRA
                        itype           := InstrType.SHIFT
                    }
                    is( B"0000000_010", B"0000000_011") {
                        // SLT, SLTU
                        itype           := InstrType.ALU
                        unsigned        := funct3(0)
                        sub             := True
                    }
              
                }
            }
        }
        is(Opcodes.SYS){
            when(funct3 === B"000"){
                itype       := InstrType.E
                iformat     := InstrFormat.I
            } otherwise {
                if(config.hasCsr){
                    itype       := InstrType.CSR
                    iformat     := InstrFormat.CSR
                    csr_use_imm := funct3(2)
                    switch(funct3){
                        is(B"001", B"101"){
                            csr_cmd := CsrCmd.WRITE
                        }
                        is(B"010", B"110"){
                            csr_cmd := CsrCmd.SET
                        }
                        is(B"011", B"111"){
                            csr_cmd := CsrCmd.CLEAR
                        }
                        default {
                            itype := InstrType.Undef
                        }
                    }
                } else {
                    itype := InstrType.Undef
                }
            }
        }
        
       
      } // end opcode 
      val i_imm = S(B((19 downto 0) -> instr(31)) ## instr(31 downto 20))
      val s_imm = S(B((19 downto 0) -> instr(31)) ## instr(31 downto 25) ## instr(11 downto 7))
      val b_imm = S(B((19 downto 0) -> instr(31)) ## instr(7) ## instr(30 downto 25) ## instr(11 downto 8) ## B"0")
      val j_imm = S(B((10 downto 0) -> instr(31)) ## instr(31) ## instr(19 downto 12) ## instr(20) ## instr(30 downto 21) ## B"0")
      val u_imm = S(instr(31 downto 12) ## B((11 downto 0) -> false))

      if(config.hasCsr){
          switch(csr_addr){
              is(U(0x300, 12 bits), U(0x304, 12 bits), U(0x305, 12 bits), U(0x340, 12 bits), U(0x341, 12 bits), U(0x342, 12 bits), U(0x344, 12 bits)){
                  csr_supported := True
              }
          }
      }

      val illegal_csr = False
      if(config.hasCsr){
          when(itype === InstrType.CSR && !csr_supported){
              illegal_csr := True
          }
      } else {
          when(itype === InstrType.CSR){
              illegal_csr := True
          }
      }

      val trap = Bool()
      trap := (itype === InstrType.Undef) || illegal_csr

    val rs1_valid =  ((iformat === InstrFormat.R) ||
                (iformat === InstrFormat.I) ||
                (iformat === InstrFormat.S) ||
                (iformat === InstrFormat.B) ||
                (iformat === InstrFormat.Shamt) ||
                ((iformat === InstrFormat.CSR) && !csr_use_imm)) && !trap

      val rs2_valid =  ((iformat === InstrFormat.R) ||
                        (iformat === InstrFormat.S) ||
                        (iformat === InstrFormat.B)    ) && !trap

      // trap is NOT included in this term because it would get up into the critical
      // path inside Fetch. So illegal instructions will result in an incorrect RAW stall, but that's
      // OK.
      val rd_valid =   ((iformat === InstrFormat.R) ||
                        (iformat === InstrFormat.I) ||
                        (iformat === InstrFormat.U) ||
                        (iformat === InstrFormat.J) ||
                        (iformat === InstrFormat.Shamt) ||
                        (iformat === InstrFormat.CSR))
      //val rd_valid_final = Bool()
      //rd_valid_final := rd_valid && !trap

      val rd_addr_final = Bits(5 bits)
      rd_addr_final :=  rd_valid ? rd_addr.asBits | B"00000"
    val RD_ADDR_FINAL = insert(rd_addr_final)
    val CSR_ADDR      = insert(csr_addr)
    val CSR_USE_IMM   = insert(csr_use_imm)
    val CSR_ZIMM      = insert(csr_zimm)
    val CSR_CMD       = insert(csr_cmd)
    val MUL_OP        = insert(mul_op)

      val rs1_33 = unsigned ? B(U(RegFile.rs1_data).resize(33)) | B(S(RegFile.rs1_data).resize(33))
      val rs2_33 = unsigned ? B(U(RegFile.rs2_data).resize(33)) | B(S(RegFile.rs2_data).resize(33))

      val op1_33 = Bits(33 bits)
      op1_33 := op1_kind.mux(
          Op1Kind.Rs1     -> rs1_33,
          Op1Kind.Zero    -> B"33'd0",
          Op1Kind.Pc      -> B(pc).resize(33)
      )
      val OP1_33 = insert(op1_33)
  
      val op2_33 = Bits(33 bits)
      op2_33 := iformat.mux(
              InstrFormat.R       -> rs2_33,
              InstrFormat.I       -> B(unsigned ? U(i_imm).resize(33) | U(i_imm.resize(33)) ),
              InstrFormat.S       -> B(s_imm.resize(33)),
              InstrFormat.U       -> B(u_imm.resize(33)),
              InstrFormat.Shamt   -> rs2_33(32 downto 5) ## instr(24 downto 20),
              default             -> rs2_33
              ) ^ B(33 bits, default -> sub)
      val OP2_33 = insert(op2_33)

      val op1_op2_lsb = B((U(False ## op1_33(7 downto 0) ## sub) + U(False ## op2_33(7 downto 0) ## sub)))(9 downto 1)
      val OP1_OP2_LSB = insert(op1_op2_lsb)

      val rs2_imm = Bits(32 bits)
      rs2_imm := iformat.mux(
              InstrFormat.I       -> RegFile.rs2_data(31 downto 21) ## i_imm(20 downto 0),
              InstrFormat.B       -> RegFile.rs2_data(31 downto 21) ## b_imm(20 downto 0),
              InstrFormat.J       -> RegFile.rs2_data(31 downto 21) ## j_imm(20 downto 0),
              default             -> RegFile.rs2_data
              )
      val RS2_IMM = insert(rs2_imm)

      val ITYPE = insert(itype)
    
      // register file
     // RegFile.rs1_rd      := rs1_valid
      RegFile.rs1_rd_addr := rs1_addr
     // RegFile.rs2_rd      := rs2_valid
      RegFile.rs2_rd_addr := rs2_addr

     val formal = if (config.hasFormal) new Area {

        when(isValid){
            rvfiOrder := rvfiOrder + 1
        }

        rvfi.valid      := isValid
        
       // when(isValid){
            rvfi.order      := rvfiOrder
            rvfi.insn       := instr
            rvfi.trap       := trap
            rvfi.halt       := False
            rvfi.intr       := False
            rvfi.rs1_addr   := rs1_valid ? rs1_addr  | 0
            rvfi.rs2_addr   := rs2_valid ? rs2_addr  | 0
            rvfi.rs1_rdata  := rs1_valid ? RegFile.rs1_data | 0
            rvfi.rs2_rdata  := rs2_valid ? RegFile.rs2_data | 0
            rvfi.rd_addr    := !trap     ? rd_addr_final.asUInt | 0
            rvfi.rd_wdata   := 0
            rvfi.pc_rdata   := pc.asUInt.resize(32)
            rvfi.pc_wdata   := 0
            rvfi.mem_addr   := 0
            rvfi.mem_rmask  := 0
            rvfi.mem_wmask  := 0
            rvfi.mem_rdata  := 0
            rvfi.mem_wdata  := 0
        //}
    } else null
  
  } // end decoder


  val exec = new execute.Area {

    import decoder._
    val itype           = InstrType()
    val instr           = Bits(32 bits)
    val funct3          = Bits(3 bits)

    itype           := ITYPE
    instr           := INSTRUCTION
    funct3          := instr(14 downto 12)   
    val op1_33      = S(OP1_33)
    val op2_33      = S(OP2_33)
    val op1_op2_lsb = OP1_OP2_LSB
    val op1         = op1_33(31 downto 0)
    val op2         = op2_33(31 downto 0)
    val rs2         = RS2_IMM
    val imm         = S(rs2(20 downto 0))
    val rd_addr     = RD_ADDR_FINAL

   // val pc = PC.asUInt.simPublic()
    val valid = Bool().simPublic()
    valid := up.isValid
    flush := False 

    val alu = new Area {

        for (stage <- List(fetch, decode)) {
           stage.throwWhen(flush , usingReady = true)
        }
        
        val rd_wr    = False
        val rd_wdata = UInt(32 bits)

        // Decode stage already op1 + op2 for lower 8 bits. Now do the upper part.
        val op_cin = op1_op2_lsb(8)

        val alu_add_33 = U((op1_33(32 downto 8) @@ op_cin) + (op2_33(32 downto 8) @@ op_cin))(25 downto 1) @@ U(op1_op2_lsb(7 downto 0))

        val rd_wdata_alu_add = alu_add_33(31 downto 0)
        val rd_wdata_alu_lt  = U(alu_add_33(32)).resize(32)

        rd_wdata := rd_wdata_alu_add

        switch(itype){
            is(InstrType.ALU_ADD){
                rd_wr    := True
                rd_wdata := rd_wdata_alu_add
            }
            is(InstrType.ALU){
                switch(funct3){
                    is(B"010",B"011"){  // SLT, SLTU
                        rd_wr    := True
                        rd_wdata := rd_wdata_alu_lt
                    }
                    is(B"100"){         // XOR
                        rd_wr    := True
                        rd_wdata := U(op1 ^ op2)
                    }
                    is(B"110"){         // OR
                        rd_wr    := True
                        rd_wdata := U(op1 | op2)
                    }
                    is(B"111"){         // AND
                        rd_wr    := True
                        rd_wdata := U(op1 & op2)
                    }
                }
            }
          }
    }

    val shift = new Area {
        val rd_wr       = (itype === InstrType.SHIFT)
        val rd_wdata    = UInt(32 bits)
        val shamt       = U(op2(4 downto 0))
        val shleft      = !funct3(2)
        val op1_33      = instr(30) ? S(op1(31) ## op1) | S(B"0" ## op1)

        rd_wdata := U(shleft ? (op1_33 |<< shamt) | (op1_33 |>> shamt))(31 downto 0)
    }

    val jump = new Area {

        val take_jump     = False
        val pc_jump_valid = False
        val pc_jump       = UInt(32 bits)
        val clr_lsb = False

        val pc       = UInt(32 bits)
        val pc_op1   = SInt(32 bits)
        pc          := PC.asUInt
        pc_op1      := S(pc)
        val pc_plus4 = pc + 4
        val rd_wr    = False
        val rd_wdata = pc_plus4.resize(32)
        when (isValid) {
          switch(itype){
            is(InstrType.B){
                val op1_eq_op2 = (op1 === op2)
                val op1_lt_op2 = alu.rd_wdata_alu_lt(0)
                val branch_cond = False
                switch(funct3){
                    is(B"000")       { branch_cond :=  op1_eq_op2 } // BEQ
                    is(B"001")       { branch_cond := !op1_eq_op2 } // BNE
                    is(B"100",B"110"){ branch_cond :=  op1_lt_op2 } // BLT, BLTU
                    is(B"101",B"111"){ branch_cond := !op1_lt_op2 } // BGE, BGEU
                }
                pc_jump_valid := True
                take_jump     := branch_cond
            }
            is(InstrType.JAL){
                pc_jump_valid := True
                take_jump     := True
                rd_wr    := True
            }
            is(InstrType.JALR){
                pc_jump_valid := True
                pc_op1        := op1
                take_jump     := True
                clr_lsb  := True
                rd_wr    := True
            }
         }
       
      }
        // Clear LSB for JALR ops
        pc_jump := (take_jump ? U(pc_op1 + imm)  |
                                pc_plus4         ) & ~(U(clr_lsb).resize(32))
        when (take_jump) {
          Iptr := pc_jump
          flush := True
        }

        
    }
  
    val lsu = new Area {

        val rd_wr    = False
        val rd_wdata = UInt(32 bits)
        val lsu_addr = UInt(32 bits)
        lsu_addr     := alu.rd_wdata_alu_add
        val size     = B(funct3(1 downto 0))
        rd_wdata     := 0
        val mem_wdata = Bits(32 bits)
        mem_wdata := 0
          
        when (itype === InstrType.S) {
           
            mem_wdata := size.mux[Bits](
            B"00" -> rs2(7 downto 0) ## rs2(7 downto 0) ## rs2(7 downto 0) ## rs2(7 downto 0),
            B"01" -> rs2(15 downto 0) ## rs2(15 downto 0),
            default -> rs2
          )                       
          memory.WriteData(lsu_addr, mem_wdata, size)
        }
        when (itype === InstrType.L) {
          memory.ReadData(lsu_addr)
           when (memory.readDone === True) {
            rd_wr    := True  
            val ld_data_signed = !funct3(2)
            val rsp_data_shift_adj = Bits(32 bits)
            rsp_data_shift_adj := memory.rdData >> (lsu_addr(1 downto 0) << 3)
            val data =  size.mux[Bits](
                            B"00"   -> (ld_data_signed ? B(S(rsp_data_shift_adj( 7 downto 0)).resize(32)) | 
                                                         B(U(rsp_data_shift_adj( 7 downto 0)).resize(32)) ),
                            B"01"   -> (ld_data_signed ? B(S(rsp_data_shift_adj(15 downto 0)).resize(32)) | 
                                                         B(U(rsp_data_shift_adj(15 downto 0)).resize(32)) ),
                            default ->  rsp_data_shift_adj
                    )
            rd_wdata := data.asUInt
           }
        }
        
    }
    val irq_taken = Bool()
    irq_taken := False
    val irq_target = UInt(32 bits)
    irq_target := U(0, 32 bits)
    val mret_taken = Bool()
    mret_taken := False
    val mret_target = UInt(32 bits)
    mret_target := U(0, 32 bits)
  
    val csrMstatusWrite = Bool()
    csrMstatusWrite := False
    val csrMstatusWmask = Bits(32 bits)
    csrMstatusWmask := B(0, 32 bits)
    val csrMstatusWdata = Bits(32 bits)
    csrMstatusWdata := B(0, 32 bits)
    val csrMepcWrite = Bool()
    csrMepcWrite := False
    val csrMepcWmask = Bits(32 bits)
    csrMepcWmask := B(0, 32 bits)
    val csrMepcWdata = Bits(32 bits)
    csrMepcWdata := B(0, 32 bits)
    val csrMcauseWrite = Bool()
    csrMcauseWrite := False
    val csrMcauseWmask = Bits(32 bits)
    csrMcauseWmask := B(0, 32 bits)
    val csrMcauseWdata = Bits(32 bits)
    csrMcauseWdata := B(0, 32 bits)

    if(config.hasCsr){
        val csrFullMask = B(BigInt("FFFFFFFF", 16), 32 bits)
        val csrPrivMask = B(BigInt("00001888", 16), 32 bits)
        val irqCauseValue = B(BigInt("8000000B", 16), 32 bits)
        val irqArea = new Area {
            val mie_meie    = CsrRegs.mie(11)
            val mip_meip    = CsrRegs.mip(11)
            val mstatus_mie = CsrRegs.mstatus(3)
            val irq_pending = mie_meie && mip_meip && mstatus_mie
            val take_irq    = irq_pending && !valid && !flush
            val mtvec_base  = (CsrRegs.mtvec(31 downto 2) ## B"00")
            val isMret      = (itype === InstrType.E) && (instr(31 downto 20) === B"001100000010")
            when(take_irq){
                irq_taken  := True
                irq_target := mtvec_base.asUInt
                CsrRegs.mepc   := Iptr.asBits
                CsrRegs.mcause := irqCauseValue
                val irqMstatusNext = Bits(32 bits)
                irqMstatusNext := CsrRegs.mstatus
                irqMstatusNext(7) := CsrRegs.mstatus(3)
                irqMstatusNext(3) := False
                irqMstatusNext(12 downto 11) := B"11"
                CsrRegs.mstatus := irqMstatusNext
                csrMepcWrite    := True
                csrMepcWmask    := csrFullMask
                csrMepcWdata    := Iptr.asBits
                csrMcauseWrite  := True
                csrMcauseWmask  := csrFullMask
                csrMcauseWdata  := irqCauseValue
                csrMstatusWrite := True
                csrMstatusWmask := csrPrivMask
                csrMstatusWdata := irqMstatusNext
            }
            when(valid && isMret){
                val resume = CsrRegs.mepc.asUInt
                mret_taken  := True
                mret_target := resume
                val mretMstatusNext = Bits(32 bits)
                mretMstatusNext := CsrRegs.mstatus
                mretMstatusNext(3) := CsrRegs.mstatus(7)
                mretMstatusNext(7) := True
                mretMstatusNext(12 downto 11) := B"00"
                CsrRegs.mstatus := mretMstatusNext
                csrMstatusWrite := True
                csrMstatusWmask := csrPrivMask
                csrMstatusWdata := mretMstatusNext
            }
        }
    }
    if(config.hasFormal && config.hasCsr){
        when(irq_taken){
            rvfiOrder := rvfiOrder + 1
        }
    }
    when(irq_taken){
        Iptr := irq_target
        flush := True
    } elsewhen(mret_taken){
        Iptr := mret_target
        flush := True
    } elsewhen(jump.take_jump){
        Iptr := jump.pc_jump
        flush := True
    }
       
   
        val mul_rd_wr = Bool()
        val mul_rd_wdata = UInt(32 bits)
        if(config.hasMul){
            val mulArea = new Area {
                val opSel      = MulOp()
                opSel         := MUL_OP
                val mulValid   = (itype === InstrType.MULDIV) && execute.isValid
                val op1_s64    = op1.resize(64)
                val op2_s64    = op2.resize(64)
                val zero32     = B((31 downto 0) -> False)
                val op1_u_s64  = S(zero32 ## op1)
                val op2_u_s64  = S(zero32 ## op2)
                val mul_ss     = (op1_s64 * op2_s64).asBits
                val mul_su     = (op1_s64 * op2_u_s64).asBits
                val mul_uu     = (U(op1).resize(64) * U(op2).resize(64)).asBits
                val rd_wr      = Bool()
                rd_wr          := False
                val rd_wdata   = UInt(32 bits)
                rd_wdata       := 0
                val result     = UInt(32 bits)
                result         := 0
                switch(opSel){
                    is(MulOp.MUL){
                        result := U(mul_ss(31 downto 0))
                    }
                    is(MulOp.MULH){
                        result := U(mul_ss(63 downto 32))
                    }
                    is(MulOp.MULHSU){
                        result := U(mul_su(63 downto 32))
                    }
                    is(MulOp.MULHU){
                        result := U(mul_uu(63 downto 32))
                    }
                }
                when(mulValid && (opSel =/= MulOp.NONE)){
                    rd_wr    := (rd_addr =/= 0)
                    rd_wdata := result
                }
            }
            mul_rd_wr    := mulArea.rd_wr
            mul_rd_wdata := mulArea.rd_wdata
        } else {
            mul_rd_wr    := False
            mul_rd_wdata := U(0, 32 bits)
        }
    val csr_rd_wr = Bool()
    val csr_rd_wdata = UInt(32 bits)
    if(config.hasCsr){
        val csrArea = new Area {
            val addr      = CSR_ADDR
            val useImm    = CSR_USE_IMM
            val zimm      = CSR_ZIMM
            val cmd       = CSR_CMD
            val operand   = op1.asBits
        
            
            when(useImm){
                operand := zimm.asBits
            }
            val writeMask = Bits(32 bits)
            
            switch(cmd.asBits){
                is(CsrCmd.WRITE)    { writeMask := 0xFFFFFFFF }
                is(CsrCmd.SET)      { writeMask := operand }
                is(CsrCmd.CLEAR)    { writeMask := operand }
                default             { writeMask := B(0, 32 bits) }
            }
            val readData = Bits(32 bits)
            readData := 0
            switch(addr.asBits){
                is(U(0x300, 12 bits)){ readData := CsrRegs.mstatus }
                is(U(0x304, 12 bits)){ readData := CsrRegs.mie }
                is(U(0x305, 12 bits)){ readData := CsrRegs.mtvec }
                is(U(0x340, 12 bits)){ readData := CsrRegs.mscratch }
                is(U(0x341, 12 bits)){ readData := CsrRegs.mepc }
                is(U(0x342, 12 bits)){ readData := CsrRegs.mcause }
                is(U(0x344, 12 bits)){ readData := CsrRegs.mip }
            }
            val writeData = Bits(32 bits)
            writeData := operand
            switch(cmd.asBits){
                is(CsrCmd.WRITE){ writeData := operand }
                is(CsrCmd.SET){ writeData := readData | operand }
                is(CsrCmd.CLEAR){ writeData := readData & ~operand }
                default{ writeData := readData }
            }
            val csrOpValid = (itype === InstrType.CSR) && execute.isValid && !decoder.trap
            val rd_wr       = Bool()
            rd_wr := False
            val rd_wdata    = UInt(32 bits)
            rd_wdata := 0
            when(csrOpValid){
                rd_wr    := (rd_addr =/= 0)
                rd_wdata := U(readData)
            }
            when(csrOpValid && (cmd =/= CsrCmd.NONE)){
                switch(addr.asBits){
                    is(U(0x300, 12 bits)){
                        CsrRegs.mstatus  := writeData
                        csrMstatusWrite := True
                        csrMstatusWmask := writeMask
                        csrMstatusWdata := writeData
                    }
                    is(U(0x304, 12 bits)){ CsrRegs.mie      := writeData }
                    is(U(0x305, 12 bits)){ CsrRegs.mtvec    := writeData }
                    is(U(0x340, 12 bits)){ CsrRegs.mscratch := writeData }
                    is(U(0x341, 12 bits)){
                        CsrRegs.mepc     := writeData
                        csrMepcWrite    := True
                        csrMepcWmask    := writeMask
                        csrMepcWdata    := writeData
                    }
                    is(U(0x342, 12 bits)){
                        CsrRegs.mcause   := writeData
                        csrMcauseWrite  := True
                        csrMcauseWmask  := writeMask
                        csrMcauseWdata  := writeData
                    }
                    is(U(0x344, 12 bits)){ CsrRegs.mip      := writeData }
                }
            }
        }
        csr_rd_wr := csrArea.rd_wr
        csr_rd_wdata := csrArea.rd_wdata
    } else {
        csr_rd_wr := False
        csr_rd_wdata := U(0, 32 bits)
    }
    val rd_wr    = execute.isValid && (alu.rd_wr | jump.rd_wr | shift.rd_wr | lsu.rd_wr | mul_rd_wr | csr_rd_wr) && (rd_addr =/= 0)
    val rd_waddr = rd_addr
    val rd_wdata = B((alu.rd_wdata.range   -> alu.rd_wr))   & B(alu.rd_wdata)   |
                   B((jump.rd_wdata.range  -> jump.rd_wr))  & B(jump.rd_wdata)  |
                   B((shift.rd_wdata.range -> shift.rd_wr)) & B(shift.rd_wdata) |
                   B((lsu.rd_wdata.range   -> lsu.rd_wr))   & B(lsu.rd_wdata)   |
                   B((mul_rd_wdata.range   -> mul_rd_wr))   & B(mul_rd_wdata)   |
                   B((csr_rd_wdata.range   -> csr_rd_wr))   & B(csr_rd_wdata)

    // register file
    RegFile.rd_wr       := rd_wr
    RegFile.rd_wr_addr  := rd_waddr.asUInt
    RegFile.rd_wr_data  := rd_wdata.asBits
    
    // FORMAL
    val formal = if (config.hasFormal) new Area {

        io.rvfi.valid := RegNext(isValid || irq_taken)

        when(isValid){
            io.rvfi.order     := rvfi.order
            io.rvfi.pc_rdata  := rvfi.pc_rdata
            io.rvfi.insn      := rvfi.insn
            io.rvfi.trap      := rvfi.trap
            io.rvfi.halt      := rvfi.halt
            io.rvfi.intr      := rvfi.intr

            io.rvfi.rs1_addr  := rvfi.rs1_addr
            io.rvfi.rs2_addr  := rvfi.rs2_addr
            io.rvfi.rd_addr   := rvfi.rd_addr

            io.rvfi.rs1_rdata := rvfi.rs1_rdata
            io.rvfi.rs2_rdata := rvfi.rs2_rdata
            io.rvfi.rd_wdata  := 0
            when (rd_wr){
                io.rvfi.rd_wdata := rd_wdata
            }
            io.rvfi.mem_addr  := 0
            io.rvfi.mem_rmask := 0
            io.rvfi.mem_rdata := 0
            io.rvfi.mem_wmask := 0
            io.rvfi.mem_wdata := 0
            io.rvfi.csr_mstatus_wmask := 0
            io.rvfi.csr_mstatus_wdata := 0
            io.rvfi.csr_mepc_wmask    := 0
            io.rvfi.csr_mepc_wdata    := 0
            io.rvfi.csr_mcause_wmask  := 0
            io.rvfi.csr_mcause_wdata  := 0
            when(csrMstatusWrite){
                io.rvfi.csr_mstatus_wmask := csrMstatusWmask
                io.rvfi.csr_mstatus_wdata := csrMstatusWdata
            }
            when(csrMepcWrite){
                io.rvfi.csr_mepc_wmask := csrMepcWmask
                io.rvfi.csr_mepc_wdata := csrMepcWdata
            }
            when(csrMcauseWrite){
                io.rvfi.csr_mcause_wmask := csrMcauseWmask
                io.rvfi.csr_mcause_wdata := csrMcauseWdata
            }
            io.rvfi.ixl      := 1
            io.rvfi.mode     := 3
        }
    
        when(isValid){
            when(jump.pc_jump_valid){
                io.rvfi.pc_wdata  := jump.pc_jump.resize(32)
            }
            .otherwise{
                io.rvfi.pc_wdata  := rvfi.pc_rdata + 4
            }
        }

        switch(itype){
            is(InstrType.B, InstrType.JAL, InstrType.JALR){
                when(isValid && jump.pc_jump_valid && !(jump.pc_jump(1 downto 0) === 0)){
                    io.rvfi.trap := True
                }
            }
            is(InstrType.L){
                when(isValid ){
                   val size     = B(funct3(1 downto 0))
                    io.rvfi.mem_addr  := lsu.lsu_addr(31 downto 2) @@ U"00"
                    io.rvfi.mem_rmask := ((size === B"00") ? B"0001" |  ((size === B"01") ? B"0011" |  B"1111")) |<< lsu.lsu_addr(1 downto 0)

                    io.rvfi.trap      := (size === B"01" && lsu.lsu_addr(0)) |
                                      (size === B"10" && !(lsu.lsu_addr(1 downto 0) === 0))
                }
            }
            is(InstrType.S){
                when(isValid ){
                   val size     = B(funct3(1 downto 0))
                    io.rvfi.mem_addr  := lsu.lsu_addr(31 downto 2) @@ U"00"
                    io.rvfi.mem_wmask := ((size === B"00") ? B"0001" |
                                      ((size === B"01") ? B"0011" |
                                                                      B"1111")) |<< lsu.lsu_addr(1 downto 0)

                    io.rvfi.mem_wdata := lsu.mem_wdata

                    io.rvfi.trap      := (size === B"01" && lsu.lsu_addr(0)) |
                                      (size === B"10" && !(lsu.lsu_addr(1 downto 0) === 0))

                }
            }
        }

        when(irq_taken){
            io.rvfi.order     := rvfiOrder
            io.rvfi.pc_rdata  := Iptr
            io.rvfi.insn      := B(BigInt("00000013", 16), 32 bits)
            io.rvfi.trap      := True
            io.rvfi.halt      := False
            io.rvfi.intr      := True
            io.rvfi.rs1_addr  := 0
            io.rvfi.rs2_addr  := 0
            io.rvfi.rd_addr   := 0
            io.rvfi.rs1_rdata := 0
            io.rvfi.rs2_rdata := 0
            io.rvfi.rd_wdata  := 0
            io.rvfi.mem_addr  := 0
            io.rvfi.mem_rmask := 0
            io.rvfi.mem_rdata := 0
            io.rvfi.mem_wmask := 0
            io.rvfi.mem_wdata := 0
            io.rvfi.pc_wdata  := irq_target
            io.rvfi.csr_mstatus_wmask := csrMstatusWmask
            io.rvfi.csr_mstatus_wdata := csrMstatusWdata
            io.rvfi.csr_mepc_wmask    := csrMepcWmask
            io.rvfi.csr_mepc_wdata    := csrMepcWdata
            io.rvfi.csr_mcause_wmask  := csrMcauseWmask
            io.rvfi.csr_mcause_wdata  := csrMcauseWdata
            io.rvfi.ixl      := 1
            io.rvfi.mode     := 3
        }

    } else null
  }
    // pipeline
  Builder(fetch, decode, execute, f2d, d2e)
  io.IO := 1
}

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
      if (!pass2) println(line)
      val tokens = line.trim.split(" ")
      val Pattern = "(.*:)".r
      val instruction = tokens(0) match {
        case "#" => // comment
        case Pattern(l) => if (!pass2) symbols += (l.substring(0, l.length - 1) -> pc)
        case "add"    =>   regNumber(tokens(3)) << 20 | regNumber(tokens(2)) << 15 | regNumber(tokens(1)) << 7 | 0x33
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
      //println(instruction)

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
                                                 supportDiv = false,
                                                 supportCsr = false))).printPruned()
}

class TopRV(config: RVConfig) extends Component {
    val io = new Bundle {
        //val osc_clk = in(Bool)

        val led1    = out(Bool)
        val led2    = out(Bool)
        val led3    = out(Bool)

        val switch = in(Bool)
        val irq    = in(Bool)
    }

    
    

    val core = new Area {

        val rv = new RV(config)        
        val cfg = MemConfig(memorySize = 1024, dataWidth = 32)
        val cpu_ram = new MultiPortMem_1w_2rs( cfg , writeFirst)
       
        // memory connections
        /// instruction memory
        rv.io.instr_req.ready   := True
        
        rv.io.instr_rsp.valid   := RegNext(rv.io.instr_req.valid) init(False)
        cpu_ram.io.rd1.addr     := (rv.io.instr_req.addr |>>2).resized
        cpu_ram.io.rd1.ena      := rv.io.instr_req.valid 
        rv.io.instr_rsp.data    := cpu_ram.io.rd1.data
        rv.io.irq               := io.irq

        // data memory
        rv.io.data_req.ready := True
        rv.io.data_rsp.valid := RegNext(rv.io.data_req.valid) init(False)




        val wmask = rv.io.data_req.size.mux(
                        B"00"   -> B"0001",
                        B"01"   -> B"0011",
                        default -> B"1111") |<< rv.io.data_req.wraddr(1 downto 0)

        cpu_ram.io.wr0.mask    := wmask

        cpu_ram.io.wr0.addr      := (rv.io.data_req.wraddr >> 2).resized
        cpu_ram.io.wr0.ena       := rv.io.data_req.valid && rv.io.data_req.wren 
        cpu_ram.io.wr0.data      := rv.io.data_req.data
        cpu_ram.io.rd0.addr      := (rv.io.data_req.rdaddr >> 2).resized
        cpu_ram.io.rd0.ena       := rv.io.data_req.valid && rv.io.data_req.rden
        rv.io.data_rsp.data      := cpu_ram.io.rd0.data.resized

        val update_leds = rv.io.data_req.valid && rv.io.data_req.wren && rv.io.data_req.wraddr(9)

        io.led1 := RegNextWhen(rv.io.data_req.data(0), update_leds) init(False)
        io.led2 := RegNextWhen(rv.io.data_req.data(1), update_leds) init(False)
        io.led3 := RegNextWhen(rv.io.data_req.data(2), update_leds) init(False)
      }

     

    
}


object RVTopVerilog extends App {
  val config=SpinalConfig(device=Device.XILINX,targetDirectory = "hw/gen",mergeAsyncProcess = true)
 
  config.generateVerilog(new TopRV(config = RVConfig(supportFormal = true,
                                                 supportMul = false,
                                                 supportDiv = false,
                                                 supportCsr = false)))
}

object RVSim extends App {
  
     val program = Assembler.assemble("asm/RV.asm")
   
     


   SimConfig.withFstWave.compile(new TopRV(config = RVConfig(supportFormal = false,
                                                 supportMul = false,
                                                 supportDiv = false,
                                                 supportCsr = false) )).doSim(seed = 2){ dut =>
  val mem = dut.core.cpu_ram
   for (i <- 0 until program.length) {
            mem.u_mem_bank0.u_mem.setBigInt(i, program(i))
            mem.u_mem_bank1.u_mem.setBigInt(i, program(i))
            
        }
  // fill the rest of memory with 0
  for (i <- program.length until 1024) {
            mem.u_mem_bank0.u_mem.setBigInt(i, 0)
            mem.u_mem_bank1.u_mem.setBigInt(i, 0)
            
        }      
  dut.clockDomain.forkStimulus(10)
  dut.io.irq #= false
  var run = 50
  // init Regfile
  for (j <- 0 until 32 ) {
    dut.core.rv.RegFile.RegMem.setBigInt(j,0)
    println()
  }

   while(run>0 && dut.core.rv.decoder.instr.toBigInt != 0 ) {
        dut.clockDomain.waitSampling(1)
        run -= 1
        if (dut.core.rv.decoder.valid.toBoolean)  {
             printf("PC: %08X, INST: %08X",dut.core.rv.decoder.pc.toBigInt, dut.core.rv.decoder.instr.toBigInt)
             println()
        }
    }
  // read value from regfile
  println("value of Regfile at the  end of simu") 
  for (j <- 0 until 32 by 2) {
    printf("x%02d:  %08X   x%02d:  %08X ", j,dut.core.rv.RegFile.RegMem.getBigInt(j),j+1,dut.core.rv.RegFile.RegMem.getBigInt(j+1))
    println()
  }
  // read value from memory
    println("value of memory at the  end of simu")
    for (j <- 14 until 32 by 2) {
        printf("mem[%02d]:  %08X   mem[%02d]:  %08X ", j,mem.u_mem_bank0.u_mem.getBigInt(j),j+1,mem.u_mem_bank0.u_mem.getBigInt(j+1))
        println()  
    } 
 }
}