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

object Op1Kind extends SpinalEnum {
    val Rs1     = newElement()
    val Zero    = newElement()
    val Pc      = newElement()
}

object CsrReg extends SpinalEnum(binaryOneHot) {
    val NONE       = newElement()
    val MSTATUS    = newElement()
    val MIE        = newElement()
    val MTVEC      = newElement()
    val MSCRATCH   = newElement()
    val MEPC       = newElement()
    val MCAUSE     = newElement()
    val MIP        = newElement()
    val DCSR       = newElement()
    val DPC        = newElement()
    val DSCRATCH0  = newElement()
    val DSCRATCH1  = newElement()
    val ILLEGAL    = newElement()
}

object MulOp extends SpinalEnum(binaryOneHot) {
    val NONE    = newElement()
    val MUL     = newElement()
    val MULH    = newElement()
    val MULHSU  = newElement()
    val MULHU   = newElement()
    val DIV     = newElement()
    val DIVU    = newElement()
    val REM     = newElement()
    val REMU    = newElement()
}

object RvfiUtils {
    def FULL_MASK = B(BigInt("FFFFFFFF", 16), 32 bits)

    /** Compute byte mask from access size (00=byte, 01=half, 10=word) */
    def sizeMask(size: Bits): Bits = {
        (size === B"00") ? B"0001" | ((size === B"01") ? B"0011" | B"1111")
    }
}

case class RvfiCsr() extends Bundle {
    val rmask = Bits(32 bits)
    val wmask = Bits(32 bits)
    val rdata = Bits(32 bits)
    val wdata = Bits(32 bits)

    def init(): this.type = {
        rmask init(0) addAttribute("keep")
        wmask init(0) addAttribute("keep")
        rdata init(0) addAttribute("keep")
        wdata init(0) addAttribute("keep")
        this
    }

    /** Clear all CSR tracking signals */
    def clear(): Unit = {
        rmask := 0; wmask := 0; rdata := 0; wdata := 0
    }

    /** Report a full CSR read+write */
    def report(readData: Bits, writeMask: Bits, writeData: Bits): Unit = {
        rmask := RvfiUtils.FULL_MASK
        rdata := readData
        wmask := writeMask
        wdata := writeData
    }

    /** Report a read-only CSR access */
    def reportRead(readData: Bits): Unit = {
        rmask := RvfiUtils.FULL_MASK
        rdata := readData
    }
}

case class RVFI() extends Bundle {

    val valid       = Bool
    val order       = UInt(64 bits)
    val insn        = Bits(32 bits)
    val insn_raw    = Bits(32 bits)
    val insn_is_c   = Bool
    val insn_len    = UInt(3 bits)
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
    val csr_mstatus = RvfiCsr()
    val csr_mepc    = RvfiCsr()
    val csr_mcause  = RvfiCsr()
    val ixl         = Bits(2 bits)
    val mode        = Bits(2 bits)

    /** Clear all CSR tracking signals */
    def clearCsrs(): Unit = {
        csr_mstatus.clear()
        csr_mepc.clear()
        csr_mcause.clear()
    }

    /** Clear memory access signals */
    def clearMem(): Unit = {
        mem_addr := 0; mem_rmask := 0; mem_rdata := 0
        mem_wmask := 0; mem_wdata := 0
    }

    /** Clear register access signals */
    def clearRegs(): Unit = {
        rs1_addr := 0; rs2_addr := 0; rd_addr := 0
        rs1_rdata := 0; rs2_rdata := 0; rd_wdata := 0
    }

    def init() : RVFI = {
        valid     init(False) addAttribute("keep")
        order     init(0) addAttribute("keep")
        insn      init(0) addAttribute("keep")
        insn_raw  init(0) addAttribute("keep")
        insn_is_c init(False) addAttribute("keep")
        insn_len  init(4) addAttribute("keep")
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
        csr_mstatus.init()
        csr_mepc.init()
        csr_mcause.init()
        ixl       init(1) addAttribute("keep")
        mode      init(3) addAttribute("keep")

        this
    }
}

case class CsrPipe() extends Bundle {
  val regidx  = CsrReg()
  val use_imm = Bool()
  val zimm    = UInt(5 bits)
  val cmd     = CsrCmd()
   def setDefault(): this.type = {
    regidx  := CsrReg.NONE
    use_imm := False
    zimm    := 0
    cmd     := CsrCmd.NONE
    this
  }
}

case class RVConfig(
                supportMulDiv   : Boolean = false,
                supportCompressed : Boolean = false,
                supportFormal   : Boolean = false,
                supportFence    : Boolean = false,
                supportDebug    : Boolean = false,
                pcSize          : Int     = 32,
                dataAddrSize    : Int     = 32,
                bootVector      : BigInt  = BigInt(0)
                ) {

    def hasMulDiv   = supportMulDiv
    def hasCompressed = supportCompressed
    def hasFence    = supportFence
    def hasDebug    = supportDebug


    def hasFormal   = supportFormal
}

object RVCDecomp {
     def main(args: Array[String]): Unit = {
    SpinalVerilog(new Component{
      out(apply(in Bits(16 bits)))
    }.setDefinitionName("Decompressor"))
  }

 
     def apply(raw: Bits): Bits = {
        val c = raw(15 downto 0)
        val cQuad = c(1 downto 0)
        val cFunct3 = c(15 downto 13)

        def sext(src: Bits, to: Int): Bits = B(S(src).resize(to))
        def encI(imm: Bits, rs1: UInt, funct3: Bits, rd: UInt, opcode: Bits): Bits =
            imm(11 downto 0) ## rs1.asBits ## funct3 ## rd.asBits ## opcode
        def encR(funct7: Bits, rs2: UInt, rs1: UInt, funct3: Bits, rd: UInt, opcode: Bits): Bits =
            funct7 ## rs2.asBits ## rs1.asBits ## funct3 ## rd.asBits ## opcode
        def encS(imm: Bits, rs2: UInt, rs1: UInt, funct3: Bits, opcode: Bits): Bits =
            imm(11 downto 5) ## rs2.asBits ## rs1.asBits ## funct3 ## imm(4 downto 0) ## opcode
        def encB(imm: Bits, rs2: UInt, rs1: UInt, funct3: Bits, opcode: Bits): Bits =
            imm(12) ## imm(10 downto 5) ## rs2.asBits ## rs1.asBits ## funct3 ## imm(4 downto 1) ## imm(11) ## opcode
        def encU(imm: Bits, rd: UInt, opcode: Bits): Bits =
            imm(31 downto 12) ## rd.asBits ## opcode
        def encJ(imm: Bits, rd: UInt, opcode: Bits): Bits =
            imm(20) ## imm(10 downto 1) ## imm(11) ## imm(19 downto 12) ## rd.asBits ## opcode

        val rd  = U(c(11 downto 7))
        val rs1p = U(B"01" ## c(9 downto 7))
        val rs2p = U(B"01" ## c(4 downto 2))

        val decomp = Bits(32 bits)
        decomp := B(0, 32 bits)

        switch(cQuad){
            is(B"00"){
                switch(cFunct3){
                    is(B"000"){
                        val imm = B(32 bits, default -> false)
                        imm(9 downto 6) := c(10 downto 7)
                        imm(5 downto 4) := c(12 downto 11)
                        imm(3) := c(5)
                        imm(2) := c(6)
                        when(c(12 downto 5) =/= 0){
                            decomp := encI(imm(11 downto 0), U(2, 5 bits), B"000", rs2p, B"0010011")
                        }
                    }
                    is(B"010"){
                        val imm = B(12 bits, default -> false)
                        imm(6) := c(5)
                        imm(5 downto 3) := c(12 downto 10)
                        imm(2) := c(6)
                        decomp := encI(imm, rs1p, B"010", rs2p, B"0000011")
                    }
                    is(B"110"){
                        val imm = B(12 bits, default -> false)
                        imm(6) := c(5)
                        imm(5 downto 3) := c(12 downto 10)
                        imm(2) := c(6)
                        decomp := encS(imm, rs2p, rs1p, B"010", B"0100011")
                    }
                }
            }
            is(B"01"){
                switch(cFunct3){
                    is(B"000"){
                        val imm = sext(c(12) ## c(6 downto 2), 12)
                        decomp := encI(imm(11 downto 0), rd, B"000", rd, B"0010011")
                    }
                    is(B"001"){
                        val jimm = sext(c(12) ## c(8) ## c(10 downto 9) ## c(6) ## c(7) ## c(2) ## c(11) ## c(5 downto 3) ## B"0", 21)
                        decomp := encJ(jimm(20 downto 0), U(1, 5 bits), B"1101111")
                    }
                    is(B"010"){
                        val imm = sext(c(12) ## c(6 downto 2), 12)
                        decomp := encI(imm(11 downto 0), U(0, 5 bits), B"000", rd, B"0010011")
                    }
                    is(B"011"){
                        when(rd === U(2, 5 bits)){
                            val imm = sext(c(12) ## c(4 downto 3) ## c(5) ## c(2) ## c(6) ## B"0000", 12)
                            decomp := encI(imm(11 downto 0), U(2, 5 bits), B"000", U(2, 5 bits), B"0010011")
                        } otherwise {
                            val uimm = sext(c(12) ## c(6 downto 2) ## B(12 bits, default -> false), 32)
                            decomp := encU(uimm, rd, B"0110111")
                        }
                    }
                    is(B"100"){
                        when(c(11 downto 10) === B"00"){
                            val shamt = B(12 bits, default -> false)
                            shamt(5) := c(12)
                            shamt(4 downto 0) := c(6 downto 2)
                            decomp := encI(shamt, rs1p, B"101", rs1p, B"0010011")
                        } elsewhen(c(11 downto 10) === B"01"){
                            val shamt = B(12 bits, default -> false)
                            shamt(5) := c(12)
                            shamt(4 downto 0) := c(6 downto 2)
                            decomp := encI(B"010000" ## shamt(5 downto 0), rs1p, B"101", rs1p, B"0010011")
                        } elsewhen(c(11 downto 10) === B"10"){
                            val imm = sext(c(12) ## c(6 downto 2), 12)
                            decomp := encI(imm(11 downto 0), rs1p, B"111", rs1p, B"0010011")
                        } otherwise {
                            when(c(12) === False){
                                switch(c(6 downto 5)){
                                    is(B"00"){ decomp := encR(B"0100000", rs2p, rs1p, B"000", rs1p, B"0110011") }
                                    is(B"01"){ decomp := encR(B"0000000", rs2p, rs1p, B"100", rs1p, B"0110011") }
                                    is(B"10"){ decomp := encR(B"0000000", rs2p, rs1p, B"110", rs1p, B"0110011") }
                                    is(B"11"){ decomp := encR(B"0000000", rs2p, rs1p, B"111", rs1p, B"0110011") }
                                }
                            }
                        }
                    }
                    is(B"101"){
                        val jimm = sext(c(12) ## c(8) ## c(10 downto 9) ## c(6) ## c(7) ## c(2) ## c(11) ## c(5 downto 3) ## B"0", 21)
                        decomp := encJ(jimm(20 downto 0), U(0, 5 bits), B"1101111")
                    }
                    is(B"110"){
                        val bimm = sext(c(12) ## c(6 downto 5) ## c(2) ## c(11 downto 10) ## c(4 downto 3) ## B"0", 13)
                        decomp := encB(bimm(12 downto 0), U(0, 5 bits), rs1p, B"000", B"1100011")
                    }
                    is(B"111"){
                        val bimm = sext(c(12) ## c(6 downto 5) ## c(2) ## c(11 downto 10) ## c(4 downto 3) ## B"0", 13)
                        decomp := encB(bimm(12 downto 0), U(0, 5 bits), rs1p, B"001", B"1100011")
                    }
                }
            }
            is(B"10"){
                switch(cFunct3){
                    is(B"000"){
                        val shamt = B(12 bits, default -> false)
                        shamt(5) := c(12)
                        shamt(4 downto 0) := c(6 downto 2)
                        decomp := encI(shamt, rd, B"001", rd, B"0010011")
                    }
                    is(B"010"){
                        val imm = B(12 bits, default -> false)
                        imm(5) := c(12)
                        imm(4 downto 2) := c(6 downto 4)
                        imm(7 downto 6) := c(3 downto 2)
                        when(rd =/= U(0, 5 bits)){
                            decomp := encI(imm, U(2, 5 bits), B"010", rd, B"0000011")
                        }
                    }
                    is(B"100"){
                        when(c(12) === False){
                            when(c(6 downto 2) === B"00000"){
                                decomp := encI(B(12 bits, default -> false), rd, B"000", U(0, 5 bits), B"1100111")
                            } otherwise {
                                decomp := encR(B"0000000", U(c(6 downto 2)), U(0, 5 bits), B"000", rd, B"0110011")
                            }
                        } otherwise {
                            when((rd === U(0, 5 bits)) && (c(6 downto 2) === B"00000")){
                                decomp := B(BigInt("00100073", 16), 32 bits)
                            } elsewhen(c(6 downto 2) === B"00000"){
                                decomp := encI(B(12 bits, default -> false), rd, B"000", U(1, 5 bits), B"1100111")
                            } otherwise {
                                decomp := encR(B"0000000", U(c(6 downto 2)), rd, B"000", rd, B"0110011")
                            }
                        }
                    }
                    is(B"110"){
                        val imm = B(12 bits, default -> false)
                        imm(5 downto 2) := c(12 downto 9)
                        imm(7 downto 6) := c(8 downto 7)
                        decomp := encS(imm, U(c(6 downto 2)), U(2, 5 bits), B"010", B"0100011")
                    }
                }
            }
        }

        decomp
    }
  

}

object RVDecode {
    case class Result() {
        val itype    = InstrType()
        val iformat  = InstrFormat()
        val op1_kind = Op1Kind()
        val sub      = Bool()
        val unsigned = Bool()
        val mul_op   = MulOp()
        val csr      = CsrPipe()
    }

    def apply(instr: Bits, config: RVConfig,
              csrAddrToEnum: UInt => SpinalEnumCraft[CsrReg.type] = null): Result = {
        val r = Result()
        val opcode = instr(6 downto 0)
        val funct3 = instr(14 downto 12)
        val funct7 = instr(31 downto 25)

        r.iformat  := InstrFormat.R
        r.itype    := InstrType.Undef
        r.op1_kind := Op1Kind.Rs1
        r.sub      := False
        r.unsigned := False
        r.mul_op   := MulOp.NONE
        r.csr.setDefault()

        switch(opcode.asBits){
            is(Opcodes.LUI){
                r.itype    := InstrType.ALU_ADD
                r.iformat  := InstrFormat.U
                r.op1_kind := Op1Kind.Zero
            }
            is(Opcodes.AUIPC){
                r.itype    := InstrType.ALU_ADD
                r.iformat  := InstrFormat.U
                r.op1_kind := Op1Kind.Pc
            }
            is(Opcodes.JAL){
                r.itype    := InstrType.JAL
                r.iformat  := InstrFormat.J
                r.op1_kind := Op1Kind.Pc
            }
            is(Opcodes.JALR){
                when(funct3 === B"000") {
                    r.itype := InstrType.JALR
                }
                r.iformat  := InstrFormat.I
            }
            is(Opcodes.B){
                when(funct3 =/= B"010" && funct3 =/= B"011") {
                    r.itype := InstrType.B
                }
                r.iformat  := InstrFormat.B
                r.unsigned := (funct3(2 downto 1) === B"11")
                r.sub      := (funct3(2 downto 1) =/= B"00")
            }
            is(Opcodes.L){
                when(funct3 =/= B"011" && funct3 =/= B"110" && funct3 =/= B"111") {
                    r.itype := InstrType.L
                }
                r.iformat  := InstrFormat.I
            }
            is(Opcodes.S){
                when(funct3 === B"000" || funct3 === B"001" || funct3 === B"010") {
                    r.itype := InstrType.S
                }
                r.iformat  := InstrFormat.S
            }
            is(Opcodes.ALUI){
                switch(funct3.asBits){
                    is(B"000"){
                        r.itype   := InstrType.ALU_ADD
                        r.iformat := InstrFormat.I
                    }
                    is(B"010", B"011") {
                        r.itype    := InstrType.ALU
                        r.iformat  := InstrFormat.I
                        r.unsigned := funct3(0)
                        r.sub      := True
                    }
                    is(B"100", B"110", B"111") {
                        r.itype   := InstrType.ALU
                        r.iformat := InstrFormat.I
                    }
                    is(B"001"){
                        when(funct7 === B"0000000"){
                            r.itype := InstrType.SHIFT
                        }
                        r.iformat := InstrFormat.Shamt
                    }
                    is(B"101"){
                        when(funct7 === B"0000000" || funct7 === B"0100000"){
                            r.itype := InstrType.SHIFT
                        }
                        r.iformat := InstrFormat.Shamt
                    }
                }
            }
            is(Opcodes.ALU){
                r.iformat := InstrFormat.R
                when(funct7 === B"0000001"){
                    switch(funct3){
                        for((f3, op) <- List(
                            B"000" -> MulOp.MUL,   B"001" -> MulOp.MULH,
                            B"010" -> MulOp.MULHSU, B"011" -> MulOp.MULHU,
                            B"100" -> MulOp.DIV,    B"101" -> MulOp.DIVU,
                            B"110" -> MulOp.REM,    B"111" -> MulOp.REMU
                        )) {
                            is(f3) { if(config.hasMulDiv) { r.itype := InstrType.MULDIV; r.mul_op := op } else { r.itype := InstrType.Undef } }
                        }
                    }
                } otherwise {
                    switch(funct7 ## funct3.asBits){
                        is(B"0000000_000", B"0100000_000"){
                            r.itype := InstrType.ALU_ADD
                            r.sub   := funct7(5)
                        }
                        is(B"0000000_100", B"0000000_110", B"0000000_111"){
                            r.itype := InstrType.ALU
                        }
                        is(B"0000000_001", B"0000000_101", B"0100000_101"){
                            r.itype := InstrType.SHIFT
                        }
                        is(B"0000000_010", B"0000000_011") {
                            r.itype    := InstrType.ALU
                            r.unsigned := funct3(0)
                            r.sub      := True
                        }
                    }
                }
            }
            is(Opcodes.SYS){
                when(funct3 === B"000"){
                    r.itype   := InstrType.E
                    r.iformat := InstrFormat.I
                } otherwise {
                    r.itype       := InstrType.CSR
                    r.iformat     := InstrFormat.CSR
                    r.csr.use_imm := funct3(2)
                    r.csr.zimm    := instr(19 downto 15).asUInt
                    switch(funct3){
                        is(B"001", B"101") { r.csr.cmd := CsrCmd.WRITE }
                        is(B"010", B"110") { r.csr.cmd := CsrCmd.SET }
                        is(B"011", B"111") { r.csr.cmd := CsrCmd.CLEAR }
                        default             { r.itype := InstrType.Undef }
                    }
                    r.csr.regidx := csrAddrToEnum(instr(31 downto 20).asUInt)
                }
            }
        }
        r
    }
}

class CsrRegFile(config: RVConfig) extends Area {
    val mstatus  = Reg(Bits(32 bits)) init(0)
    val mtvec    = Reg(Bits(32 bits)) init(0)
    val mscratch = Reg(Bits(32 bits)) init(0)
    val mepc     = Reg(Bits(32 bits)) init(0)
    val mcause   = Reg(Bits(32 bits)) init(0)
    val mie      = Reg(Bits(32 bits)) init(0)
    val mip      = Reg(Bits(32 bits)) init(0)
    val dcsr     = if(config.hasDebug) Reg(Bits(32 bits)) init(B(BigInt("40000003", 16), 32 bits)) else null
    val dpc      = if(config.hasDebug) Reg(Bits(32 bits)) init(0) else null
    val dscratch0 = if(config.hasDebug) Reg(Bits(32 bits)) init(0) else null
    val dscratch1 = if(config.hasDebug) Reg(Bits(32 bits)) init(0) else null

    val csrMap: Seq[(Int, SpinalEnumElement[CsrReg.type], Bits)] = Seq(
        (0x300, CsrReg.MSTATUS,  mstatus),
        (0x304, CsrReg.MIE,      mie),
        (0x305, CsrReg.MTVEC,    mtvec),
        (0x340, CsrReg.MSCRATCH, mscratch),
        (0x341, CsrReg.MEPC,     mepc),
        (0x342, CsrReg.MCAUSE,   mcause),
        (0x344, CsrReg.MIP,      mip)
    ) ++ (if(config.hasDebug) Seq(
        (0x7B0, CsrReg.DCSR,      dcsr),
        (0x7B1, CsrReg.DPC,       dpc),
        (0x7B2, CsrReg.DSCRATCH0, dscratch0),
        (0x7B3, CsrReg.DSCRATCH1, dscratch1)
    ) else Nil)

    def read(sel: SpinalEnumCraft[CsrReg.type]): Bits = {
        val result = Bits(32 bits)
        result := 0
        switch(sel) { for((_, e, r) <- csrMap) is(e) { result := r } }
        result
    }

    def write(sel: SpinalEnumCraft[CsrReg.type], data: Bits): Unit = {
        switch(sel) {
            for((_, e, r) <- csrMap) is(e) {
                if(e == CsrReg.MIP) { for(i <- 0 until 32 if i != 11 && i != 7) r(i) := data(i) }
                else { r := data }
            }
        }
    }

    def addrToEnum(addr: UInt): SpinalEnumCraft[CsrReg.type] = {
        val result = CsrReg()
        result := CsrReg.NONE
        switch(addr) {
            for((a, e, _) <- csrMap) is(U(a, 12 bits)) { result := e }
            default { result := CsrReg.ILLEGAL }
        }
        result
    }
}

case class DebugBus() extends Bundle {
    val halt_req   = in Bool()
    val resume_req = in Bool()
    val halted     = out Bool()
    val reg_addr   = in UInt(5 bits)
    val reg_wdata  = in Bits(32 bits)
    val reg_rdata  = out Bits(32 bits)
    val reg_wr     = in Bool()
    val csr_addr   = in UInt(12 bits)
    val csr_wdata  = in Bits(32 bits)
    val csr_rdata  = out Bits(32 bits)
    val csr_wr     = in Bool()
}

class DebugController(config: RVConfig, csrRegs: CsrRegFile, regMem: Mem[Bits], debugIo: DebugBus) extends Area {
    val halted = RegInit(False)
    val mode   = RegInit(False)

    // Report halted status
    debugIo.halted := halted

    // GPR read (combinational)
    debugIo.reg_rdata := Mux(debugIo.reg_addr === 0, B(0, 32 bits),
                             regMem.readAsync(debugIo.reg_addr))
    // GPR write (when halted)
    when(halted && debugIo.reg_wr && debugIo.reg_addr =/= 0) {
        regMem.write(debugIo.reg_addr, debugIo.reg_wdata)
    }

    // CSR read/write via debug interface
    val debugCsrSel = csrRegs.addrToEnum(debugIo.csr_addr)
    debugIo.csr_rdata := csrRegs.read(debugCsrSel)
    when(halted && debugIo.csr_wr) {
        csrRegs.write(debugCsrSel, debugIo.csr_wdata)
    }

    /** Drive halt / resume / ebreak / dret state machine.
      * Must be called after IRQ/jump logic so debug overrides have priority. */
    def update(valid: Bool, isEbreak: Bool, isDret: Bool,
               pc: Bits, nextTrapPc: UInt, flush: Bool, Iptr: UInt): Unit = {
        // External halt request – wait for pipeline idle (!valid) for clean halt
        when(debugIo.halt_req && !halted && !valid) {
            halted := True
            mode   := True
            csrRegs.dpc := nextTrapPc.asBits
            csrRegs.dcsr(8 downto 6) := B"011"  // cause = 3 (halt request)
            flush := True
        }
        // ebreak enters debug mode when dcsr.ebreakm is set or already in debug mode
        val ebreakToDebug = isEbreak && valid && (mode || csrRegs.dcsr(15))
        when(ebreakToDebug) {
            halted := True
            mode   := True
            csrRegs.dpc := pc
            csrRegs.dcsr(8 downto 6) := B"001"  // cause = 1 (ebreak)
            flush := True
        }
        // Resume request
        when(debugIo.resume_req && halted) {
            halted := False
            mode   := False
            Iptr := csrRegs.dpc.asUInt
            flush := True
        }
        // dret instruction exits debug mode
        when(valid && isDret && mode) {
            mode := False
            Iptr := csrRegs.dpc.asUInt
            flush := True
        }
    }
}

class RV (config: RVConfig) extends Component {
 
  val fetch, decode, execute = CtrlLink()
  val f2d = StageLink(fetch.down, decode.up)
  val d2e = StageLink(decode.down, execute.up)
  // payload
  val INSTRUCTION = Payload(Bits(32 bits))
  val DECODED_INSTR = Payload(Bits(32 bits))  // decompressed instruction for execute
  val PC = Payload(Bits(32 bits))
  val rvfi = if (config.hasFormal) Payload(RVFI()) else null
  val rvfiOrder = if (config.hasFormal) Reg(UInt(64 bits)) init(0) else null

  // I/O
  val io = new Bundle {
    val IO = out(UInt(32 bits)).simPublic()
    val rvfi = if (config.hasFormal) out(Reg(RVFI()) init).setName("rvfi") else null
    //val exit = out(Reg(Bool)).init(False)

    val instr_axi = master(AxiLite4ReadOnly(
        AxiLite4Config(
            addressWidth = config.pcSize,
            dataWidth    = 32
        )
    )).setName("instr_axi")

    val data_axi  = master(AxiLite4(
        AxiLite4Config(
            addressWidth = config.dataAddrSize,
            dataWidth    = 32
        )
    )).setName("data_axi")
    val irq       = in(Bool).setName("irq")
    val timer_irq = in(Bool).setName("timer_irq")

    val debug = if(config.hasDebug) DebugBus().setName("debug") else null
  }


  // general
    val Iptr = Reg(UInt(32 bits)) init(U(config.bootVector, 32 bits))
    val nextTrapPc = Reg(UInt(32 bits)) init(U(config.bootVector, 32 bits))
    val flush = Bool()
    val coreinit = RegInit(False)
    coreinit := True  
    val irqSyncStage0 = RegNext(io.irq).init(False)
    val irqSync       = RegNext(irqSyncStage0).init(False)
    val timerIrqSyncStage0 = RegNext(io.timer_irq).init(False)
    val timerIrqSync       = RegNext(timerIrqSyncStage0).init(False)
  
  
  // register file
  val RegFile =  new Area {

    val rs1_rd_addr = UInt(5 bits)
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
    rs1_data := Mux(rs1_rd_addr === 0, B(0, 32 bits), Mux(rd_wr && rs1_rd_addr === rd_wr_addr, rd_wr_data, rs1))
    rs2_data := Mux(rs2_rd_addr === 0, B(0, 32 bits), Mux(rd_wr && rs2_rd_addr === rd_wr_addr, rd_wr_data, rs2))
    RegMem.write(rd_wr_addr, rd_wr_data, rd_wr)
  }

    val CsrRegs = new CsrRegFile(config)
    CsrRegs.mip(11) := irqSync
    CsrRegs.mip(7)  := timerIrqSync

    val dbg = if(config.hasDebug) new DebugController(config, CsrRegs, RegFile.RegMem, io.debug) else null
  
 
    //mem
    val dataMemory = new Area {
        val readDone = Bool
        val writeDone = Bool
        val rdData   = Bits(32 bits)
        val loadPending = RegInit(False) addAttribute("keep")
        val writePending  = RegInit(False) addAttribute("keep")

        rdData   := io.data_axi.r.data
        readDone := io.data_axi.r.fire
        writeDone := io.data_axi.b.fire
        when(io.data_axi.r.fire) { loadPending := False }
        when(io.data_axi.ar.fire) { loadPending := True }
        when(io.data_axi.w.fire ) { writePending := True }
        when(io.data_axi.b.fire) { writePending := False }

        // default values
        io.data_axi.aw.valid := False
        io.data_axi.aw.addr  := 0
        io.data_axi.aw.prot  := B"000"
        io.data_axi.w.valid  := False
        io.data_axi.w.data   := 0
        io.data_axi.w.strb   := 0
        io.data_axi.ar.valid := False
        io.data_axi.ar.addr  := 0
        io.data_axi.ar.prot  := B"000"
        io.data_axi.b.ready  := True
        io.data_axi.r.ready  := True

        def WriteData(addr: UInt, data: Bits, size: Bits) = {
            io.data_axi.aw.addr := addr.resized
            io.data_axi.aw.valid  := !writePending
            io.data_axi.aw.prot  := B"000"
            io.data_axi.w.strb := size.mux(
                B"00"   -> B"0001",
                B"01"   -> B"0011",
                default -> B"1111") |<< addr(1 downto 0)
            io.data_axi.w.data    := data
            io.data_axi.w.valid   := !writePending
        }
        def ReadData(addr: UInt) = {
            io.data_axi.ar.addr := addr.resized
            io.data_axi.ar.valid  := !loadPending
            io.data_axi.ar.prot  := B"000"
        }
    }

    val instrMemory = new Area {
  
        val loadPending = RegInit(False) addAttribute("keep")

        val rdInst   = Reg(Bits(32 bits)) init(0)
        val rdPc     = Reg(Bits(32 bits)) init(0)
        val PcReg    = Reg(UInt(32 bits)) init(0)
        val rdValid   = RegInit(False)
        val loadDone = Bool()
        val instrReqValid = Bool() addAttribute("keep")

        val pcFifo = new StreamFifo(UInt(32 bits), depth = 2,
            withBypass = true,
            withAsyncRead = true,
            useVec = true
        )

        // default values
        io.instr_axi.ar.valid := False
        io.instr_axi.ar.addr  := 0
        io.instr_axi.ar.prot  := B"000"
        io.instr_axi.r.ready  := True

        loadDone := io.instr_axi.r.fire
        when(io.instr_axi.ar.fire) { loadPending := True }
        when(!io.instr_axi.ar.valid) { loadPending := False }

        def Fetch(addr: UInt) = {
            io.instr_axi.ar.addr := addr.resized
            io.instr_axi.ar.valid  := /*!loadPending && */ instrReqValid && !flush
            io.instr_axi.ar.prot  := B"000"
            PcReg := addr
        }

        pcFifo.io.flush        := flush
        pcFifo.io.push.valid   := io.instr_axi.ar.fire
        pcFifo.io.push.payload := io.instr_axi.ar.addr
        pcFifo.io.pop.ready    := loadDone

        when(flush) {
            loadDone     := False
            loadPending  := False
            rdValid      := False
        }
        when (loadDone && coreinit){
            rdInst := io.instr_axi.r.data
            rdPc   := pcFifo.io.pop.payload.asBits
            //rdPc   := PcReg.asBits
            rdValid := True
        } otherwise {
            rdValid := False    
        }
    }
  
    val fetcher = new fetch.Area {
        case class FetchPacket() extends Bundle {
            val inst = Bits(32 bits)
            val pc   = Bits(32 bits)
        }

        val fetchFifo = new StreamFifo(FetchPacket(), depth = 4,
            withBypass = true,
            withAsyncRead = true,
            useVec = true
        )

        // Common fetch logic: memory requests → FIFO push
        val rvcStop = if(config.hasCompressed) RegInit(False) else False
        val dbgHalt = if(config.hasDebug) (dbg.halted || io.debug.halt_req) else False
        val canRequest = (fetchFifo.io.availability > 2) && RegNext(coreinit) && fetchFifo.io.push.ready && !rvcStop && !dbgHalt && instrMemory.pcFifo.io.push.ready
        instrMemory.instrReqValid := canRequest
        when (canRequest) {
            instrMemory.Fetch(Iptr)
        }
        when(io.instr_axi.ar.fire) {
            Iptr := Iptr + 4
        }
        fetchFifo.io.push.valid          := instrMemory.rdValid
        fetchFifo.io.push.payload.inst   := instrMemory.rdInst
        fetchFifo.io.push.payload.pc     := instrMemory.rdPc
        fetchFifo.io.flush               := flush
     
        val rvc = if(config.hasCompressed) new Area {
            val case2a = Bool() 
            val stop = rvcStop.asInstanceOf[Bool]
            fetchFifo.io.pop.ready := down.ready && !stop

            // split fifo out into high and low half
            val lo = fetchFifo.io.pop.payload.inst(15 downto 0)
            val hi = fetchFifo.io.pop.payload.inst(31 downto 16)
            val instr_hi_is_rvi = (hi(1 downto 0) === B"11") // 32b instruction
            val instr_lo_is_rvi = (lo(1 downto 0) === B"11") // 32b instruction
            val instr_push = Bits(32 bits)
            val unaligned_lo = Reg(Bits(16 bits)) init(0)
            val pc_unaligned = Reg(Bool()) init(False)
            val instr32b_unaligned = Reg(Bool()) init(False)
            val instr16b_unaligned = Reg(Bool()) init(False)
            val savedPc = Reg(UInt(32 bits)) init(0)
            
            // 4  cases for compressed instructions:
            // 1. lo is 32b -> push 32b 
            // 2a. lo is 16b and hi is 16b   -> push lo, save PC, stop FIFO
            // 2b. lo is 16b and hi is 16b   -> push hi from register, resume FIFO
            // 3. lo is 16b and hi is 32b ( split in two words ) -> push lo then store hi for next cycle
            // 4. lo is the hi part half of the previously unaligned 32b instruction -> push reconstructed 32b 
            when (down.ready && (fetchFifo.io.pop.valid || instr16b_unaligned)) {
                // Only update unaligned_lo when pipeline advances,
                // to avoid corruption during stalls (hi changes due to FIFO pop)
                unaligned_lo := hi

                when (instr32b_unaligned) { // case 4
                    instr_push := lo ## unaligned_lo  
                    when (instr_hi_is_rvi) {
                        pc_unaligned := True
                        instr32b_unaligned := True
                        instr16b_unaligned := False
                        stop := False
                    } otherwise { 
                        pc_unaligned := True
                        instr32b_unaligned := False
                        instr16b_unaligned := True
                        savedPc := fetchFifo.io.pop.payload.pc.asUInt
                        stop := True
                    }
                } elsewhen (instr_lo_is_rvi && !instr16b_unaligned ) { // case 1
                    instr_push := hi ## lo
                    pc_unaligned := False
                    instr32b_unaligned := False
                    instr16b_unaligned := False
                    stop := False
                } elsewhen  (instr_hi_is_rvi && !instr16b_unaligned) { // case 3
                    instr_push := lo.resized
                    pc_unaligned := True
                    instr32b_unaligned := True
                    instr16b_unaligned := False
                    stop := False
                } otherwise { // case 2
                    when (instr16b_unaligned) { // case 2b
                        instr_push := unaligned_lo.resized
                        pc_unaligned := False
                        instr32b_unaligned := False
                        instr16b_unaligned := False
                        stop := False
                    } otherwise { // case 2a
                        instr_push := lo.resized
                        pc_unaligned := True
                        instr32b_unaligned := False
                        instr16b_unaligned := True
                        savedPc := fetchFifo.io.pop.payload.pc.asUInt
                        stop := True
                    }
                    
                }
            } otherwise {
                instr_push := 0
            }
            case2a := instr16b_unaligned
            val pc_push = UInt(32 bits)
            // In case 2b, use saved PC + 2; otherwise derive from FIFO
            pc_push := instr16b_unaligned ? (savedPc + 2) | 
                       (fetchFifo.io.pop.payload.pc.asUInt - (pc_unaligned ? U(2, 32 bits) | U(0, 32 bits)))

            // case 2b: instruction comes from register, valid even when FIFO is stopped
            up.valid    := fetchFifo.io.pop.valid || instr16b_unaligned
            INSTRUCTION := instr_push
            PC          := pc_push.asBits

            when(flush) {
                stop := False
                instr16b_unaligned := False
                instr32b_unaligned := False
                pc_unaligned := False
            }

        } else new Area {
            fetchFifo.io.pop.ready := down.ready

            up.valid    := fetchFifo.io.pop.valid
            INSTRUCTION := fetchFifo.io.pop.payload.inst
            PC          := fetchFifo.io.pop.payload.pc
        }


       
    }

    val decoder = new decode.Area {
    
    val instr      = Bits(32 bits).simPublic()
    val rawInstr   = INSTRUCTION.asBits
    instr := rawInstr
    val cIs16      = if(config.hasCompressed) (rawInstr(1 downto 0) =/= B"11") else False
    val cIllegal   = Bool

   

    if(config.hasCompressed){
        val decompInstr = RVCDecomp(rawInstr)
        cIllegal := cIs16 && (decompInstr === 0)
        when(cIs16){
            instr := decompInstr
        }
    } else {
        cIllegal := False
    }

    val instrIsCompressed = Bool()
    if(config.hasCompressed){
        instrIsCompressed := cIs16
    } else {
        instrIsCompressed := False
    }

    val pc         = PC.asBits.simPublic()
    val rd_addr     = U(instr(11 downto 7))
    val rs1_addr    = U(instr(19 downto 15))
    val rs2_addr    = U(instr(24 downto 20))

    val valid = Bool().simPublic()
    valid := up.isValid

    val dec = RVDecode(instr, config, CsrRegs.addrToEnum _)
    val itype    = dec.itype
    val iformat  = dec.iformat
    val op1_kind = dec.op1_kind
    val sub      = dec.sub
    val unsigned = dec.unsigned
    val mul_op   = dec.mul_op
    val csr      = dec.csr
    val CSR      = insert(csr)

    val i_imm = S(B((19 downto 0) -> instr(31)) ## instr(31 downto 20))
    val s_imm = S(B((19 downto 0) -> instr(31)) ## instr(31 downto 25) ## instr(11 downto 7))
    val b_imm = S(B((19 downto 0) -> instr(31)) ## instr(7) ## instr(30 downto 25) ## instr(11 downto 8) ## B"0")
    val j_imm = S(B((10 downto 0) -> instr(31)) ## instr(31) ## instr(19 downto 12) ## instr(20) ## instr(30 downto 21) ## B"0")
    val u_imm = S(instr(31 downto 12) ## B((11 downto 0) -> false))

    val trap = Bool()
    trap := (itype === InstrType.Undef) || (csr.regidx === CsrReg.ILLEGAL) || cIllegal

    val rs1_valid =  ((iformat === InstrFormat.R) ||
                (iformat === InstrFormat.I) ||
                (iformat === InstrFormat.S) ||
                (iformat === InstrFormat.B) ||
                (iformat === InstrFormat.Shamt) ||
                ((iformat === InstrFormat.CSR) && !csr.use_imm)) && !trap

      val rs2_valid =  ((iformat === InstrFormat.R) ||
                        (iformat === InstrFormat.S) ||
                        (iformat === InstrFormat.B)    ) && !trap

      
      val rd_valid =   ((iformat === InstrFormat.R) ||
                        (iformat === InstrFormat.I) ||
                        (iformat === InstrFormat.U) ||
                        (iformat === InstrFormat.J) ||
                        (iformat === InstrFormat.Shamt) ||
                        (iformat === InstrFormat.CSR))&& !trap
      //val rd_valid_final = Bool()
      //rd_valid_final := rd_valid && !trap

      val rd_addr_final = Bits(5 bits)
      rd_addr_final :=  rd_valid ? rd_addr.asBits | B"00000"
    val RD_ADDR_FINAL = insert(rd_addr_final)

    DECODED_INSTR := instr   // propagate decompressed instruction to execute
   
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
    val IS_C = insert(instrIsCompressed)
    val TRAP = insert(trap)
    
      // register file
     // RegFile.rs1_rd      := rs1_valid
      RegFile.rs1_rd_addr := rs1_addr
     // RegFile.rs2_rd      := rs2_valid
      RegFile.rs2_rd_addr := rs2_addr

     val formal = if (config.hasFormal) new Area {
            rvfi.order      := rvfiOrder
            rvfi.insn       := instrIsCompressed ? (B(0, 16 bits) ## rawInstr(15 downto 0)) | instr
            rvfi.insn_raw   := rawInstr
            rvfi.insn_is_c  := instrIsCompressed
            rvfi.insn_len   := instrIsCompressed ? U(2, 3 bits) | U(4, 3 bits)
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
            rvfi.clearMem()
            
        //}
    } else null
  
  } // end decoder


    val exec = new execute.Area {

    import decoder._
    val itype           = InstrType()
    val instr           = Bits(32 bits).simPublic()
    val funct3          = Bits(3 bits)

    itype           := ITYPE
    instr           := DECODED_INSTR
    funct3          := instr(14 downto 12)   
    val op1_33      = S(OP1_33)
    val op2_33      = S(OP2_33)
    val op1_op2_lsb = OP1_OP2_LSB
    val op1         = op1_33(31 downto 0)
    val op2         = op2_33(31 downto 0)
    val rs2         = RS2_IMM
    val imm         = S(rs2(20 downto 0))
    val instrStep   = UInt(32 bits)
    instrStep       := IS_C ? U(2, 32 bits) | U(4, 32 bits)
    val rd_addr     = RD_ADDR_FINAL
    val haltRequest = if(config.hasDebug) dbg.halted else False

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
        val pc_plusInc = pc + instrStep
        val rd_wr    = False
        val rd_wdata = pc_plusInc.resize(32)
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
                                                                pc_plusInc       ) & ~(U(clr_lsb).resize(32))
        // Do not write link register when JAL/JALR target is misaligned (trap)
        // Exception: C.J/C.JAL specs don't check misalignment, so don't suppress for them
        val jumpMisaligned = if(config.hasCompressed) pc_jump(0) else !(pc_jump(1 downto 0) === 0)
        when(pc_jump_valid && jumpMisaligned && !(IS_C && itype === InstrType.JAL)) {
          rd_wr := False
        }
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
        
        val memRdData = Bits(32 bits)  
        memRdData := 0
        val memMisaligned = (size === B"01" && lsu_addr(0)) ||
                            (size === B"10" && !(lsu_addr(1 downto 0) === 0))
        // halt until memory operation is done (but not for misaligned accesses - those trap immediately)
        haltWhen(!dataMemory.readDone && itype === InstrType.L && !memMisaligned)
        haltWhen(!dataMemory.writeDone && itype === InstrType.S && !memMisaligned)

        when (itype === InstrType.S && valid && !memMisaligned) {   
                  mem_wdata := size.mux[Bits](
                    B"00" -> rs2(7 downto 0) ## rs2(7 downto 0) ## rs2(7 downto 0) ## rs2(7 downto 0),
                    B"01" -> rs2(15 downto 0) ## rs2(15 downto 0),
                    default -> rs2)
                   dataMemory.WriteData(lsu_addr, mem_wdata, size)
   
        }
        when (itype === InstrType.L && valid && !memMisaligned) {
           dataMemory.ReadData(lsu_addr)
           when (dataMemory.readDone === True) {
            rd_wr    := True  
            val ld_data_signed = !funct3(2)
            val rsp_data_shift_adj = Bits(32 bits)
            memRdData := dataMemory.rdData
            rsp_data_shift_adj := memRdData >> (lsu_addr(1 downto 0) << 3)
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
    
    val irq = new Area {
    
        val irq_taken = False
        val irq_target = U(0, 32 bits)
        val mret_taken = False
        val mret_target = U(0, 32 bits)
        val irqCauseExternal = B(BigInt("8000000B", 16), 32 bits)
        val irqCauseTimer    = B(BigInt("80000007", 16), 32 bits)
        val mie_meie    = CsrRegs.mie(11)
        val mip_meip    = CsrRegs.mip(11)
        val mie_mtie    = CsrRegs.mie(7)
        val mip_mtip    = CsrRegs.mip(7)
        val mstatus_mie = CsrRegs.mstatus(3)
        val ext_irq_pending   = mie_meie && mip_meip
        val timer_irq_pending = mie_mtie && mip_mtip
        val irq_pending = (ext_irq_pending || timer_irq_pending) && mstatus_mie
        val irqCauseValue = ext_irq_pending ? irqCauseExternal | irqCauseTimer
        val take_irq    = irq_pending && !valid && !(if(config.hasDebug) dbg.halted else False)
        val mtvec_base  = (CsrRegs.mtvec(31 downto 2) ## B"00")
        val isMret      = (itype === InstrType.E) && (instr(31 downto 20) === B"001100000010")
        when(take_irq){
            irq_taken  := True
            irq_target := mtvec_base.asUInt
            CsrRegs.mepc   := nextTrapPc.asBits
            CsrRegs.mcause := irqCauseValue
            val irqMstatusNext = Bits(32 bits)
            irqMstatusNext := CsrRegs.mstatus
            irqMstatusNext(7) := CsrRegs.mstatus(3)
            irqMstatusNext(3) := False
            irqMstatusNext(12 downto 11) := B"11"
            CsrRegs.mstatus := irqMstatusNext
        
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

        }
            
    }
    when(irq.irq_taken){
        Iptr := irq.irq_target
        flush := True
        nextTrapPc := irq.irq_target
    } elsewhen(irq.mret_taken){
        Iptr := irq.mret_target
        flush := True
        nextTrapPc := irq.mret_target
    } elsewhen(jump.take_jump){
        Iptr := jump.pc_jump
        flush := True
        nextTrapPc := jump.pc_jump
    }

    // Common instruction decode for debug/formal
    val isEbreak = if(config.hasDebug) (itype === InstrType.E) && (instr(31 downto 20) === B"000000000001") else False
    val isDret   = if(config.hasDebug) (itype === InstrType.E) && (instr === B(BigInt("7B200073", 16), 32 bits)) else False

    // Debug halt / resume / ebreak / dret
    if(config.hasDebug) {
        dbg.update(valid, isEbreak, isDret, pc.asBits, nextTrapPc, flush, Iptr)
    }

    val execRetire = execute.isValid && !flush && !haltRequest && (!dataMemory.loadPending || dataMemory.readDone)
    when(execRetire && !jump.take_jump){
        nextTrapPc := pc + instrStep
    }
       
   
    val mul =  if(config.hasMulDiv) new Area {
            val opSel      = MulOp()
            opSel         := MUL_OP
            val mulValid   = (itype === InstrType.MULDIV) && execute.isValid
            val op1_s64    = op1.resize(64)
            val op2_s64    = op2.resize(64)
            val zero32     = B((31 downto 0) -> False)
            val op2_u_s64  = S(zero32 ## op2)
            val mul_ss     = (op1_s64 * op2_s64).asBits
            val mul_su     = (op1_s64 * op2_u_s64).asBits
            val mul_uu     = (U(op1).resize(64) * U(op2).resize(64)).asBits

            val divByZero       = (op2 === 0)
            val signedOverflow  = (op1.asBits === B(BigInt("80000000", 16), 32 bits)) && (op2.asBits === B(BigInt("FFFFFFFF", 16), 32 bits))
            val divSignedNorm   = (op1 / op2).asBits
            val divUnsignedNorm = (op1.asUInt / op2.asUInt).asBits
            val remSignedNorm   = (op1 % op2).asBits
            val remUnsignedNorm = (op1.asUInt % op2.asUInt).asBits

            val divSignedRes = Bits(32 bits)
            val divUnsignedRes = Bits(32 bits)
            val remSignedRes = Bits(32 bits)
            val remUnsignedRes = Bits(32 bits)

            divSignedRes := divByZero ? B(BigInt("FFFFFFFF", 16), 32 bits) |
                    (signedOverflow ? B(BigInt("80000000", 16), 32 bits) | divSignedNorm)
            divUnsignedRes := divByZero ? B(BigInt("FFFFFFFF", 16), 32 bits) | divUnsignedNorm
            remSignedRes := divByZero ? op1.asBits |
                    (signedOverflow ? B(0, 32 bits) | remSignedNorm)
            remUnsignedRes := divByZero ? op1.asBits | remUnsignedNorm

            val rd_wr      = False
            val rd_wdata   = U(0,32 bits)
            val result     = U(0, 32 bits)
          
            if (config.hasFormal) {
                // RISCV_FORMAL_ALTOPS: replace real mul/div with simple XOR-based operations
                val altops_add = U((op1 + op2).asBits)
                val altops_sub = U((op1 - op2).asBits)
                switch(opSel){
                    is(MulOp.MUL)    { result := altops_add ^ U(BigInt("5876063e", 16), 32 bits) }
                    is(MulOp.MULH)   { result := altops_add ^ U(BigInt("f6583fb7", 16), 32 bits) }
                    is(MulOp.MULHSU) { result := altops_sub ^ U(BigInt("ecfbe137", 16), 32 bits) }
                    is(MulOp.MULHU)  { result := altops_add ^ U(BigInt("949ce5e8", 16), 32 bits) }
                    is(MulOp.DIV)    { result := altops_sub ^ U(BigInt("7f8529ec", 16), 32 bits) }
                    is(MulOp.DIVU)   { result := altops_sub ^ U(BigInt("10e8fd70", 16), 32 bits) }
                    is(MulOp.REM)    { result := altops_sub ^ U(BigInt("8da68fa5", 16), 32 bits) }
                    is(MulOp.REMU)   { result := altops_sub ^ U(BigInt("3138d0e1", 16), 32 bits) }
                }
            } else {
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
                    is(MulOp.DIV){
                        result := U(divSignedRes)
                    }
                    is(MulOp.DIVU){
                        result := U(divUnsignedRes)
                    }
                    is(MulOp.REM){
                        result := U(remSignedRes)
                    }
                    is(MulOp.REMU){
                        result := U(remUnsignedRes)
                    }
                }
            }
            when(mulValid && (opSel =/= MulOp.NONE)){
                rd_wr    := (rd_addr =/= 0)
                rd_wdata := result
            }
        }
        else new Area { // no multiplier, tie off signals       
                val rd_wr    = False
                val rd_wdata = U(0, 32 bits)
        }
  
    
    // CSR write details exposed for RVFI
    val csrRvfi = if(config.hasFormal) new Area {
        val opValid   = Bool()
        val writeMask = B(0, 32 bits)
        val writeData = B(0, 32 bits)
        val readData  = B(0, 32 bits)
    } else null

    val csr = new Area {
        val operand   = Bits(32 bits)
        operand := Mux(CSR.use_imm, B(CSR.zimm.resize(32)), op1.asBits)
        val readData = CsrRegs.read(CSR.regidx)
        val writeMask = Bits(32 bits)
        val writeData = Bits(32 bits)
        writeMask := 0
        writeData := readData
        switch(CSR.cmd){
            is(CsrCmd.WRITE)    { writeData := operand ;               writeMask := RvfiUtils.FULL_MASK }
            is(CsrCmd.SET)      { writeData := readData | operand ;    writeMask := operand }
            is(CsrCmd.CLEAR)    { writeData := readData & ~operand  ;  writeMask := operand }
            default              { writeData := readData; writeMask := B(0, 32 bits) }
        }
        
        val csrOpValid = (itype === InstrType.CSR) && execute.isValid && !decoder.TRAP
        val rd_wr       = False
        val rd_wdata    = U(0,32 bits)
        
        when(csrOpValid){
            rd_wr    := (rd_addr =/= 0)
            rd_wdata := U(readData)
        }
        when(csrOpValid && (CSR.cmd =/= CsrCmd.NONE)){
            CsrRegs.write(CSR.regidx, writeData)
        }
        // Expose CSR write details for RVFI
        if(config.hasFormal) {
            csrRvfi.opValid := csrOpValid && (CSR.cmd =/= CsrCmd.NONE)
            when(csrOpValid && (CSR.cmd =/= CsrCmd.NONE)) {
                csrRvfi.writeMask := writeMask
                csrRvfi.writeData := writeData
            }
            when(csrOpValid) {
                csrRvfi.readData := readData
            }
        }
    }
    
    val rd_wr    = execute.isValid && (alu.rd_wr | jump.rd_wr | shift.rd_wr | lsu.rd_wr | mul.rd_wr | csr.rd_wr) && (rd_addr =/= 0)
    val rd_waddr = rd_addr
    val rd_wdata = B((alu.rd_wdata.range   -> alu.rd_wr))   & B(alu.rd_wdata)   |
                   B((jump.rd_wdata.range  -> jump.rd_wr))  & B(jump.rd_wdata)  |
                   B((shift.rd_wdata.range -> shift.rd_wr)) & B(shift.rd_wdata) |
                   B((lsu.rd_wdata.range   -> lsu.rd_wr))   & B(lsu.rd_wdata)   |
                   B((mul.rd_wdata.range   -> mul.rd_wr))   & B(mul.rd_wdata)   |
                   B((csr.rd_wdata.range   -> csr.rd_wr))   & B(csr.rd_wdata)

    // register file
    RegFile.rd_wr       := rd_wr
    RegFile.rd_wr_addr  := rd_waddr.asUInt
    RegFile.rd_wr_data  := rd_wdata.asBits
    
    // FORMAL
    val formal = if (config.hasFormal) new Area {
        
        val execPc = PC.asUInt.resize(32)
        
        val rvfiRetire = execute.isValid && !haltRequest && (itype =/= InstrType.L || dataMemory.readDone || lsu.memMisaligned) && (itype =/= InstrType.S || dataMemory.writeDone || lsu.memMisaligned)

        when(rvfiRetire){
            rvfiOrder := rvfiOrder + 1
        }

        io.rvfi.valid := rvfiRetire
        when(isValid){
            io.rvfi.order     := rvfiOrder
            io.rvfi.pc_rdata  := execPc
            io.rvfi.insn      := rvfi.insn
            io.rvfi.insn_raw  := rvfi.insn_raw
            io.rvfi.insn_is_c := rvfi.insn_is_c
            io.rvfi.insn_len  := rvfi.insn_len
            io.rvfi.trap      := rvfi.trap
            io.rvfi.halt      := rvfi.halt
            if(config.hasDebug) {
                // EBREAK entering debug halt: report halt=1 so liveness checker knows CPU stopped
                when(isEbreak && (dbg.mode || CsrRegs.dcsr(15))) {
                    io.rvfi.halt := True
                }
            }
            io.rvfi.intr      := rvfi.intr

            io.rvfi.rs1_addr  := rvfi.rs1_addr
            io.rvfi.rs2_addr  := rvfi.rs2_addr
            io.rvfi.rd_addr   := rd_wr ? rvfi.rd_addr | U(0, 5 bits)

            io.rvfi.rs1_rdata := rvfi.rs1_rdata
            io.rvfi.rs2_rdata := rvfi.rs2_rdata
            io.rvfi.rd_wdata  := 0
            when (rd_wr){
                io.rvfi.rd_wdata := rd_wdata
            }
            io.rvfi.clearMem()
            io.rvfi.clearCsrs()
           
            io.rvfi.ixl      := 1
            io.rvfi.mode     := 3
        }

        // CSR write tracking for CSR instructions
        when(csrRvfi.opValid) {
                switch(CSR.regidx) {
                    is(CsrReg.MSTATUS) { io.rvfi.csr_mstatus.report(csrRvfi.readData, csrRvfi.writeMask, csrRvfi.writeData) }
                    is(CsrReg.MEPC)    { io.rvfi.csr_mepc.report(csrRvfi.readData, csrRvfi.writeMask, csrRvfi.writeData) }
                    is(CsrReg.MCAUSE)  { io.rvfi.csr_mcause.report(csrRvfi.readData, csrRvfi.writeMask, csrRvfi.writeData) }
                }
            }
            // mret modifies mstatus
            when(irq.mret_taken) {
                val mretMstatus = Bits(32 bits)
                mretMstatus := CsrRegs.mstatus
                mretMstatus(3) := CsrRegs.mstatus(7)
                mretMstatus(7) := True
                mretMstatus(12 downto 11) := B"00"
                io.rvfi.csr_mstatus.report(CsrRegs.mstatus, RvfiUtils.FULL_MASK, mretMstatus)
                io.rvfi.csr_mepc.reportRead(CsrRegs.mepc)
            }
    
        when(isValid){
            when(jump.take_jump){
                io.rvfi.pc_wdata  := jump.pc_jump.resize(32)
            }
            .otherwise{
                io.rvfi.pc_wdata  := execPc + instrStep
            }
        }

        // pc_wdata overrides for mret, ebreak, dret
        when(irq.mret_taken) {
            io.rvfi.pc_wdata := irq.mret_target
        }
        if(config.hasDebug) {
            when(isEbreak && valid && (dbg.mode || CsrRegs.dcsr(15))) {
                io.rvfi.pc_wdata := execPc   // ebreak halts at current PC
            }
            when(valid && isDret && dbg.mode) {
                io.rvfi.pc_wdata := CsrRegs.dpc.asUInt
            }
        }

        switch(itype){
            is(InstrType.B, InstrType.JAL, InstrType.JALR){
                val misaligned = if(config.hasCompressed) jump.pc_jump(0) else !(jump.pc_jump(1 downto 0) === 0)
                // C.J/C.JAL specs don't check misalignment; all other jump/branch specs do
                when(isValid && jump.pc_jump_valid && misaligned && !(IS_C && itype === InstrType.JAL)){
                    io.rvfi.trap := True
                }
            }
            is(InstrType.L){
                when(isValid ){
                    io.rvfi.trap := lsu.memMisaligned
                    when(!lsu.memMisaligned) {
                        io.rvfi.mem_addr  := lsu.lsu_addr(31 downto 2) @@ U"00"
                        io.rvfi.mem_rmask := RvfiUtils.sizeMask(lsu.size) |<< lsu.lsu_addr(1 downto 0)
                        when(dataMemory.readDone){
                            io.rvfi.mem_rdata := lsu.memRdData
                        }
                    }
                }
            }
            is(InstrType.S){
                when(isValid ){
                    io.rvfi.trap := lsu.memMisaligned
                    when(!lsu.memMisaligned) {
                        io.rvfi.mem_addr  := lsu.lsu_addr(31 downto 2) @@ U"00"
                        io.rvfi.mem_wmask := RvfiUtils.sizeMask(lsu.size) |<< lsu.lsu_addr(1 downto 0)
                        io.rvfi.mem_wdata := lsu.mem_wdata
                    }
                }
            }
        }

        when(irq.irq_taken){
            io.rvfi.valid     := True
            rvfiOrder         := rvfiOrder + 1
            io.rvfi.order     := rvfiOrder
            io.rvfi.pc_rdata  := Iptr
            io.rvfi.insn      := B(0, 32 bits)
            io.rvfi.insn_raw  := B(0, 32 bits)
            io.rvfi.insn_is_c := False
            io.rvfi.insn_len  := U(4, 3 bits)
            io.rvfi.trap      := True
            io.rvfi.halt      := False
            io.rvfi.intr      := True
            io.rvfi.clearRegs()
            io.rvfi.clearMem()
            io.rvfi.pc_wdata  := irq.irq_target

            // IRQ writes mepc, mcause, mstatus
            val irqMstatus = Bits(32 bits)
            irqMstatus := CsrRegs.mstatus
            irqMstatus(7) := CsrRegs.mstatus(3)
            irqMstatus(3) := False
            irqMstatus(12 downto 11) := B"11"
            io.rvfi.csr_mstatus.report(CsrRegs.mstatus, RvfiUtils.FULL_MASK, irqMstatus)
            io.rvfi.csr_mepc.report(CsrRegs.mepc, RvfiUtils.FULL_MASK, nextTrapPc.asBits)
            val irqIsExternal = CsrRegs.mie(11) && CsrRegs.mip(11)
            val irqCauseData = irqIsExternal ? B(BigInt("8000000B", 16), 32 bits) | B(BigInt("80000007", 16), 32 bits)
            io.rvfi.csr_mcause.report(CsrRegs.mcause, RvfiUtils.FULL_MASK, irqCauseData)

            io.rvfi.ixl      := 1
            io.rvfi.mode     := 3
        }

    } else null
  }
    // pipeline
  Builder(fetch, decode, execute, f2d, d2e)
  io.IO := 1
 
}

