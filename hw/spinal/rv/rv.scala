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
        ixl       init(1) addAttribute("keep")
        mode      init(3) addAttribute("keep")

        this
    }
}
class RV (hasFormal : Boolean = false) extends Component {
 
  val fetch, decode, execute = CtrlLink()
  val f2d = StageLink(fetch.down, decode.up)
  val d2e = StageLink(decode.down, execute.up)
  // payload
  val INSTRUCTION = Payload(Bits(32 bits))
  val PC = Payload(Bits(32 bits))
  val rvfi = if (hasFormal) Payload(RVFI()) else null

  // general
  val Iptr = Reg(UInt(32 bits)) init(0)
  val flush = Bool()
  val init = RegInit(False)
  init := True  
  val isFetch = True
  val isData = False
  
  // register file
  val RegFile =  new Area {
    val rs1_rd      = Bool
    val rs1_rd_addr = UInt(5 bits)
    val rs2_rd      = Bool
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
  
  val io = new Bundle {
    val IO = out(UInt(32 bits)).simPublic()
    val rvfi = if (hasFormal) out(Reg(RVFI()) init).setName("rvfi") else null
    //val exit = out(Reg(Bool)).init(False)
    }
  //mem
  val memory = new Area {
    var delay = 1 // delay for read access 
    val rdDelay = Reg(UInt(2 bits)) init (delay)
    val done = rdDelay===0
    val rdData = Bits(32 bits)
    val rdInst = Bits(32 bits)
    
    val wrData = B"h00000000"
    val wrEn = False
    val rdAddrD = U"0000000000"
    val rdAddrI = U"0000000000"
    val wrAddr = U"0000000000"
    
    when (!init) {
      isFetch := False
    }
    val memConfig = MemConfig(memorySize = 1024, dataWidth = 32)
    val mem = new MultiPortMem_1w_2rs(memConfig, writeFirst)
    mem.io.wr0.addr:= wrAddr
    mem.io.wr0.data:= wrData
    mem.io.wr0.ena := wrEn

    mem.io.rd1.addr:= rdAddrD
    mem.io.rd1.ena := isData
    rdData := mem.io.rd1.data

    mem.io.rd0.addr := rdAddrI
    mem.io.rd0.ena := isFetch
    rdInst  := mem.io.rd0.data
 
    def WriteData(addr: UInt, data: Bits, stall : Boolean = false ) = {
      wrAddr := addr.resized
      wrData := data
      wrEn := True
      if (stall) {
        execute.haltIt()
      }
    }
    def ReadData(addr: UInt) = {
      rdAddrD := addr.resized
      isData := True
    }

    when(isData === True) {
      rdDelay := rdDelay - 1 
      when (rdDelay > 0 ) (execute.haltIt())
    }
    when(rdDelay === 0) {
        rdDelay := delay
      }
   }  
  
  val fetcher = new fetch.Area {
    val fifo= new StreamFifo(Bits(32 bits),  2,
        withBypass = true,
        withAsyncRead = true,
        useVec = true
      ) 
    val delayFiring = RegNext (up.isFiring)
    val delayFiring2 = RegNext (delayFiring)
    
    up.valid := RegNext(isFetch).init(False)
    // pc
     when(up.isFiring ) (Iptr := Iptr + 4)
     when (init) (memory.rdAddrI := (Iptr |>> 2).resized)
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
        
       
      } // end opcode 
      val i_imm = S(B((19 downto 0) -> instr(31)) ## instr(31 downto 20))
      val s_imm = S(B((19 downto 0) -> instr(31)) ## instr(31 downto 25) ## instr(11 downto 7))
      val b_imm = S(B((19 downto 0) -> instr(31)) ## instr(7) ## instr(30 downto 25) ## instr(11 downto 8) ## B"0")
      val j_imm = S(B((10 downto 0) -> instr(31)) ## instr(31) ## instr(19 downto 12) ## instr(20) ## instr(30 downto 21) ## B"0")
      val u_imm = S(instr(31 downto 12) ## B((11 downto 0) -> false))

      val trap = (itype === InstrType.Undef)

      val rs1_valid =  ((iformat === InstrFormat.R) ||
                        (iformat === InstrFormat.I) ||
                        (iformat === InstrFormat.S) ||
                        (iformat === InstrFormat.B) ||
                        (iformat === InstrFormat.Shamt)) && !trap

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
                        (iformat === InstrFormat.Shamt))
      val rd_valid_final = Bool()
      rd_valid_final := rd_valid && !trap

      val rd_addr_final = Bits(5 bits)
      rd_addr_final :=  rd_valid ? rd_addr.asBits | B"00000"
      val RD_ADDR_FINAL = insert(rd_addr_final)

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
      RegFile.rs1_rd      := rs1_valid
      RegFile.rs1_rd_addr := rs1_addr
      RegFile.rs2_rd      := rs2_valid
      RegFile.rs2_rd_addr := rs2_addr

     val formal = if (hasFormal) new Area {

        val order = Reg(UInt(64 bits)) init(0)
        when(isValid){
            order := order + 1
        }

        rvfi.valid      := isValid
        
       // when(isValid){
            rvfi.order      := order
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
    

    


    val pc = PC.asUInt.simPublic()
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

       // val lsu_stall = Bool

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
          memory.WriteData(lsu_addr, mem_wdata)
        }
        when (itype === InstrType.L) {
          memory.ReadData(lsu_addr)
           when (memory.done === True) {
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

    val rd_wr    = decoder.down.valid && (alu.rd_wr | jump.rd_wr | shift.rd_wr | lsu.rd_wr ) && (rd_addr =/= 0)
    val rd_waddr = rd_addr
    val rd_wdata = B((alu.rd_wdata.range   -> alu.rd_wr))   & B(alu.rd_wdata)   |
                   B((jump.rd_wdata.range  -> jump.rd_wr))  & B(jump.rd_wdata)  |
                   B((shift.rd_wdata.range -> shift.rd_wr)) & B(shift.rd_wdata) |
                   B((lsu.rd_wdata.range   -> lsu.rd_wr))   & B(lsu.rd_wdata)

    // register file
  
    RegFile.rd_wr       := rd_wr
    RegFile.rd_wr_addr  := rd_waddr.asUInt
    RegFile.rd_wr_data  := rd_wdata.asBits
    
    // FORMAL
    val formal = if (hasFormal) new Area {

        io.rvfi.valid := isValid

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
                when(isValid){
                   val size     = B(funct3(1 downto 0))
                    io.rvfi.mem_addr  := lsu.lsu_addr(31 downto 2) @@ U"00"
                    io.rvfi.mem_rmask := ((size === B"00") ? B"0001" |
                                      ((size === B"01") ? B"0011" |
                                                                      B"1111")) |<< lsu.lsu_addr(1 downto 0)

                    io.rvfi.trap      := (size === B"01" && lsu.lsu_addr(0)) |
                                      (size === B"10" && !(lsu.lsu_addr(1 downto 0) === 0))
                }
            }
            is(InstrType.S){
                when(isValid){
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

    } else null
  }
  
  
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
        case "lhu"   =>   regNumber(tokens(3)) << 15 | toInt(tokens(2)) << 20  | 0x5 <<12 | regNumber(tokens(1)) << 7 | 0x03
       
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
  config.generateVerilog(new RV(true))
}
object RVSim extends App {
  
     val program = Assembler.assemble("asm/RV.asm")
     //val program = Assembler.assemble("asm/tp2.asm")
     


   SimConfig.withFstWave.compile(new RV(true)).doSim(seed = 2){ dut =>
  val mem = dut.memory.mem
   for (i <- 0 until program.length) {
            mem.u_mem_bank0.u_mem.setBigInt(i, program(i))
        }
  // fill the rest of memory with 0
  for (i <- program.length until 1024) {
            mem.u_mem_bank0.u_mem.setBigInt(i, 0)
        }      
  dut.clockDomain.forkStimulus(10)
  var run = 20
  // init Regfile
  for (j <- 0 until 32 ) {
    dut.RegFile.RegMem.setBigInt(j,0)
    println()
  }

   while(run>0 && dut.decoder.instr.toBigInt != 0 ) {
        dut.clockDomain.waitSampling(1)
        run -= 1
        if (dut.decoder.valid.toBoolean)  {
             printf("PC: %08X, INST: %08X",dut.decoder.pc.toBigInt, dut.decoder.instr.toBigInt)
             println()
        }
    }
  // read value from mem
  println("value of Regfile at the  end of simu") 
  for (j <- 0 until 32 by 2) {
    printf("x%02d:  %08X   x%02d:  %08X ", j,dut.RegFile.RegMem.getBigInt(j),j+1,dut.RegFile.RegMem.getBigInt(j+1))
    println()
  }
  

  //assert A is 0x0
  //assert(dut.A.toBigInt == 0,"A shall be zero at the end of a test case.\n")
 }
}