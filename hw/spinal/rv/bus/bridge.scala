package rv.bus.axi4lite
import spinal.core._
import spinal.lib._ 
import spinal.lib.bus.amba3.apb._


/** Minimal AXI4Lite -> APB3 bridge.
  * axiAddrWidth : width of the AXI address bus (e.g. 32)
  * apbAddrWidth : width of the APB3 PADDR 
  * One read or write in flight at a time (AXI4Lite rule). */
class Axi4LiteToApb3Bridge(axiAddrWidth: Int, apbAddrWidth: Int, dataWidth: Int) extends Component {
    val apbCfg = Apb3Config(addressWidth = apbAddrWidth, dataWidth = dataWidth, useSlaveError = false)
    val axiCfg = Axi4LiteConfig(addressWidth = axiAddrWidth, dataWidth = dataWidth, useProt = false)
    val io = new Bundle {
        val axi = slave (Axi4Lite(axiCfg))
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
class Axi4LiteDecoder(axiConfig: Axi4LiteConfig,
                      addrMap  : Seq[(BigInt, BigInt)]) extends Component {
    val n = addrMap.length
    val io = new Bundle {
        val input   = slave  (Axi4Lite(axiConfig))
        val outputs = Vec(master(Axi4Lite(axiConfig)), n)
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

    val arSlaveReady = (0 until n).foldLeft(False: Bool)((acc, i) =>
        acc || (slaveOf(io.input.ar.addr)(i) && io.outputs(i).ar.ready)
    )
    io.input.ar.ready := !rPending && arSlaveReady
    for (i <- 0 until n) {
        io.outputs(i).ar.valid := io.input.ar.valid && slaveOf(io.input.ar.addr)(i) && !rPending
        io.outputs(i).ar.addr  := io.input.ar.addr
        if(axiConfig.useProt) { io.outputs(i).ar.prot  := io.input.ar.prot }
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

    val awSlaveReady = (0 until n).foldLeft(False: Bool)((acc, i) =>
        acc || (slaveOf(io.input.aw.addr)(i) && io.outputs(i).aw.ready)
    )
    io.input.aw.ready := !wPending && awSlaveReady
    for (i <- 0 until n) {
        io.outputs(i).aw.valid := io.input.aw.valid && slaveOf(io.input.aw.addr)(i) && !wPending
        io.outputs(i).aw.addr  := io.input.aw.addr
        if(axiConfig.useProt) { io.outputs(i).aw.prot  := io.input.aw.prot }
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


