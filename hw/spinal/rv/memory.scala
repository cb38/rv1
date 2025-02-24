package multiport_memory
import spinal.core._
import spinal.lib._
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

    override def asMaster(): Unit = {
        out(ena, addr, data)
    }
}

class Mem_1w_1rs(config: MemConfig, readUnderWrite: ReadUnderWritePolicy = dontCare) extends Component
{
    val io = new Bundle {
        val wr_ena    = in(Bool)
        val wr_addr   = in(UInt(config.addrWidth bits))
        val wr_data   = in(Bits(config.dataWidth bits))

        val rd_ena    = in(Bool)
        val rd_addr   = in(UInt(config.addrWidth bits))
        val rd_data   = out(Bits(config.dataWidth bits))
    }

    val u_mem = Mem(Bits(config.dataWidth bits), wordCount = config.memorySize).simPublic()
    u_mem.write(
        enable    = io.wr_ena,
        address   = io.wr_addr,
        data      = io.wr_data
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

    u_mem_bank1.io.rd_ena    <> io.rd1.ena
    u_mem_bank1.io.rd_addr   <> io.rd1.addr
    u_mem_bank1.io.rd_data   <> io.rd1.data

}





