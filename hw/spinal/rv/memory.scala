package multiport_memory
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import spinal.sim._
import spinal.core.sim._

case class MemConfig(
    memorySize      : Int,
    dataWidth       : Int
  )
{

    def addrWidth = log2Up(memorySize)
}

case class MemRd(config: MemConfig) extends Bundle with IMasterSlave
{
    val ena         = Bool
    val addr        = UInt(config.addrWidth bits)
    val data        = Bits(config.dataWidth bits)

    override def asMaster(): Unit = {
        out(ena, addr)
        in(data)
    }
}

case class MemWr(config: MemConfig) extends Bundle with IMasterSlave
{
    val ena         = Bool
    val addr        = UInt(config.addrWidth bits)
    val data        = Bits(config.dataWidth bits)
    val mask        = Bits(config.dataWidth/8 bits)

    override def asMaster(): Unit = {
        out(ena, addr, data, mask)
    }
}

class Mem_1w_1rs(config: MemConfig, readUnderWrite: ReadUnderWritePolicy = dontCare) extends Component
{
    val io = new Bundle {
        val wr_ena    = in(Bool)
        val wr_addr   = in(UInt(config.addrWidth bits))
        val wr_data   = in(Bits(config.dataWidth bits))
        val wr_mask   = in(Bits(config.dataWidth/8 bits))

        val rd_ena    = in(Bool)
        val rd_addr   = in(UInt(config.addrWidth bits))
        val rd_data   = out(Bits(config.dataWidth bits))
    }

    val u_mem = Mem(Bits(config.dataWidth bits), wordCount = config.memorySize).simPublic()
    u_mem.write(
        enable    = io.wr_ena,
        address   = io.wr_addr,
        data      = io.wr_data,
        mask      = io.wr_mask
    )

    val rd_data_mem = u_mem.readSync(
        enable    = io.rd_ena,
        address   = io.rd_addr
    )

    if (readUnderWrite == dontCare || readUnderWrite == readFirst)
        io.rd_data := rd_data_mem
    else {
        val rd_eq_wr = io.wr_addr === io.rd_addr

        val bypass_ena_p1 = RegNext(io.wr_ena && rd_eq_wr)
        val wr_data_p1    = RegNextWhen(io.wr_data, io.wr_ena && io.rd_ena && rd_eq_wr)

        io.rd_data := bypass_ena_p1 ? wr_data_p1 | rd_data_mem
    }
}


class MultiPortMem_1w_2rs(config: MemConfig, readUnderWrite: ReadUnderWritePolicy = dontCare) extends Component {
    val io = new Bundle {
        val wr0     = slave(MemWr(config))
        val rd0     = slave(MemRd(config))
        val rd1     = slave(MemRd(config))
    }

    // Clock wr transaction for forwarding (writeFirst mode: read gets the new write value)
    val wr0_ena_p1  = RegNext(io.wr0.ena)
    val wr0_addr_p1 = RegNext(io.wr0.addr)
    val wr0_data_p1 = RegNext(io.wr0.data)

    //============================================================
    // RD0 Port RAM
    //============================================================
    val u_mem_bank0 = new Mem_1w_1rs(config, readUnderWrite)
    u_mem_bank0.io.wr_ena    <> io.wr0.ena
    u_mem_bank0.io.wr_addr   <> io.wr0.addr
    u_mem_bank0.io.wr_data   <> io.wr0.data
    u_mem_bank0.io.wr_mask   <> io.wr0.mask

    u_mem_bank0.io.rd_ena    <> io.rd0.ena
    u_mem_bank0.io.rd_addr   <> io.rd0.addr
    u_mem_bank0.io.rd_data   <> io.rd0.data

    //============================================================
    // RD1 Port RAM
    //============================================================
    //
    val u_mem_bank1 = new Mem_1w_1rs(config, readUnderWrite)
    u_mem_bank1.io.wr_ena    <> io.wr0.ena
    u_mem_bank1.io.wr_addr   <> io.wr0.addr
    u_mem_bank1.io.wr_data   <> io.wr0.data
    u_mem_bank1.io.wr_mask   <> io.wr0.mask

    u_mem_bank1.io.rd_ena    <> io.rd1.ena
    u_mem_bank1.io.rd_addr   <> io.rd1.addr
    u_mem_bank1.io.rd_data   <> io.rd1.data

}



class AXI4Lite_Mem(axiLiteCfg: AxiLite4Config, config: MemConfig) extends Component
{


    private val bytesPerWord = axiLiteCfg.dataWidth / 8
    private val addrLsb      = log2Up(bytesPerWord)
   
    
    private def toWordAddress(addr: UInt): UInt = {
        if(addrLsb == 0) addr.resize(config.addrWidth)
        else (addr >> addrLsb).resized
    }

    val io = new Bundle {
        val axi = slave(AxiLite4(axiLiteCfg))
    }

    val mem = new Mem_1w_1rs(config, readUnderWrite = dontCare)

    // -----------------------------
    // Write channel (single outstanding)
    // -----------------------------
    val awPending = RegInit(False)
    val wPending  = RegInit(False)
    val bValid    = RegInit(False)

    val wrAddrReg = Reg(UInt(config.addrWidth bits)) init(0)
    val wrDataReg = Reg(Bits(config.dataWidth bits)) init(0)
    val wrMaskReg = Reg(Bits(bytesPerWord bits)) init(0)

    io.axi.aw.ready := !awPending
    when(io.axi.aw.fire){
        wrAddrReg := toWordAddress(io.axi.aw.addr)
        awPending := True
    }

    io.axi.w.ready := !wPending
    when(io.axi.w.fire){
        wrDataReg := io.axi.w.data
        wrMaskReg := io.axi.w.strb
        wPending  := True
    }

    val writeFire = awPending && wPending && !bValid
    when(writeFire){
        awPending := False
        wPending  := False
        bValid    := True
    }

    io.axi.b.valid := bValid
    io.axi.b.resp  := B"00" // OKAY
    when(io.axi.b.fire){
        bValid := False
    }

    mem.io.wr_ena  := writeFire
    mem.io.wr_addr := wrAddrReg
    mem.io.wr_data := wrDataReg
    mem.io.wr_mask := wrMaskReg

    // -----------------------------
    // Read channel (single outstanding)
    // -----------------------------
    val rdBusy     = RegInit(False)
    val rdAddrReg  = Reg(UInt(config.addrWidth bits)) init(0)
    val rdDataReg  = Reg(Bits(config.dataWidth bits)) init(0)
    val rdValidReg = RegInit(False)

    io.axi.ar.ready := !rdBusy
    val rdLaunch    = io.axi.ar.fire
    val rdAddrNext  = toWordAddress(io.axi.ar.addr)

    when(rdLaunch){
        rdBusy    := True
        rdAddrReg := rdAddrNext
    }
    when(io.axi.r.fire){
        rdBusy     := False
        rdValidReg := False
    }

    val rdAddrForMem = rdLaunch ? rdAddrNext | rdAddrReg
    mem.io.rd_ena  := rdLaunch
    mem.io.rd_addr := rdAddrForMem

    val rdLatency = RegNext(rdLaunch, init = False)
    when(rdLatency){
        rdDataReg  := mem.io.rd_data
        rdValidReg := True
    }

    io.axi.r.valid := rdValidReg
    io.axi.r.data  := rdDataReg
    io.axi.r.resp  := B"00" // OKAY
}

class AXI4LiteReadOnly_Mem(axiLiteCfg: AxiLite4Config, config: MemConfig) extends Component
{


    private val bytesPerWord = axiLiteCfg.dataWidth / 8
    private val addrLsb      = log2Up(bytesPerWord)
   
    
    private def toWordAddress(addr: UInt): UInt = {
        if(addrLsb == 0) addr.resize(config.addrWidth)
        else (addr >> addrLsb).resized
    }

    val io = new Bundle {
        val axi = slave(AxiLite4ReadOnly(axiLiteCfg))
    }

    val mem = new Mem_1w_1rs(config, readUnderWrite = dontCare)

   
    // default values for write port
    mem.io.wr_ena  := False
    mem.io.wr_addr := 0
    mem.io.wr_data := 0
    mem.io.wr_mask := 0

    mem.io.rd_ena  := io.axi.ar.valid
    mem.io.rd_addr := toWordAddress(io.axi.ar.addr)
    

    io.axi.ar.ready := True
    io.axi.r.valid := RegNext(io.axi.ar.valid)
    io.axi.r.data  := mem.io.rd_data
    io.axi.r.resp  := B"00" // OKAY
}