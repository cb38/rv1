package rv
import spinal.core._
import spinal.lib._

//
// RISC-V Debug Module 
//
// Implements the Debug Module (DM) per RISC-V Debug Spec 0.13.
// Provides a DMI (Debug Module Interface) bus for external access
// and connects to the core's debug port for halt/resume and
// register/CSR access via abstract commands.
//
// DMI address map:
//   0x04: data0       - Abstract command data register
//   0x10: dmcontrol   - Debug Module control
//   0x11: dmstatus    - Debug Module status (read-only)
//   0x16: abstractcs  - Abstract command status
//   0x17: command     - Abstract command (write triggers execution)
//
// Abstract commands supported:
//   cmdtype=0 (Access Register):
//     - GPR x0-x31  (regno 0x1000-0x101F)
//     - CSR         (regno 0x0000-0x0FFF)
//     - aarsize=2   (32-bit only)
//

// ============================================================
// DMI bus bundle (directly from DTM or testbench)
// ============================================================
case class DmiBus() extends Bundle with IMasterSlave {
    val req_valid  = Bool()
    val req_addr   = UInt(7 bits)
    val req_data   = Bits(32 bits)
    val req_op     = Bits(2 bits)   // 1=read, 2=write
    val resp_valid = Bool()
    val resp_data  = Bits(32 bits)

    override def asMaster(): Unit = {
        out(req_valid, req_addr, req_data, req_op)
        in(resp_valid, resp_data)
    }
}

// ============================================================
// JTAG Debug Transport Module (DTM)
//
// Implements the JTAG TAP controller per IEEE 1149.1 and the
// RISC-V Debug Spec 0.13 DTM registers.
//
// JTAG IR (5-bit):
//   0x01 = IDCODE   (32-bit, read-only)
//   0x10 = DTMCS    (32-bit, control/status)
//   0x11 = DMI      (41-bit: addr[6:0] | data[31:0] | op[1:0])
//   0x1F = BYPASS   (1-bit)
//
// The DTM converts DMI shift register captures/updates into
// DMI bus transactions to the DebugModule, with CDC from the
// TCK domain to the system clock domain.
// ============================================================
class JtagDTM(idcodeValue: BigInt = BigInt("DEADBEEF", 16)) extends Component {
    val io = new Bundle {
        // JTAG pins
        val tck    = in Bool()
        val tms    = in Bool()
        val tdi    = in Bool()
        val tdo    = out Bool()
        val trst_n = in Bool()

        // DMI bus output (directly to DebugModule, same clock domain as system)
        val dmi = master(DmiBus())
    }

    // ---- JTAG TAP state machine (runs in TCK domain) ----
    val tckDomain = ClockDomain(
        clock = io.tck,
        reset = io.trst_n,
        config = ClockDomainConfig(
            clockEdge = RISING,
            resetKind = ASYNC,
            resetActiveLevel = LOW
        )
    )

    val tckArea = new ClockingArea(tckDomain) {
        // TAP states
        object TapState extends SpinalEnum {
            val TEST_LOGIC_RESET, RUN_TEST_IDLE,
                SELECT_DR, CAPTURE_DR, SHIFT_DR, EXIT1_DR, PAUSE_DR, EXIT2_DR, UPDATE_DR,
                SELECT_IR, CAPTURE_IR, SHIFT_IR, EXIT1_IR, PAUSE_IR, EXIT2_IR, UPDATE_IR = newElement()
        }
        import TapState._

        val tapState = RegInit(TEST_LOGIC_RESET)

        switch(tapState) {
            is(TEST_LOGIC_RESET) { tapState := io.tms.mux(TEST_LOGIC_RESET, RUN_TEST_IDLE) }
            is(RUN_TEST_IDLE)    { tapState := io.tms.mux(SELECT_DR, RUN_TEST_IDLE) }
            is(SELECT_DR)        { tapState := io.tms.mux(SELECT_IR, CAPTURE_DR) }
            is(CAPTURE_DR)       { tapState := io.tms.mux(EXIT1_DR, SHIFT_DR) }
            is(SHIFT_DR)         { tapState := io.tms.mux(EXIT1_DR, SHIFT_DR) }
            is(EXIT1_DR)         { tapState := io.tms.mux(UPDATE_DR, PAUSE_DR) }
            is(PAUSE_DR)         { tapState := io.tms.mux(EXIT2_DR, PAUSE_DR) }
            is(EXIT2_DR)         { tapState := io.tms.mux(UPDATE_DR, SHIFT_DR) }
            is(UPDATE_DR)        { tapState := io.tms.mux(SELECT_DR, RUN_TEST_IDLE) }
            is(SELECT_IR)        { tapState := io.tms.mux(TEST_LOGIC_RESET, CAPTURE_IR) }
            is(CAPTURE_IR)       { tapState := io.tms.mux(EXIT1_IR, SHIFT_IR) }
            is(SHIFT_IR)         { tapState := io.tms.mux(EXIT1_IR, SHIFT_IR) }
            is(EXIT1_IR)         { tapState := io.tms.mux(UPDATE_IR, PAUSE_IR) }
            is(PAUSE_IR)         { tapState := io.tms.mux(EXIT2_IR, PAUSE_IR) }
            is(EXIT2_IR)         { tapState := io.tms.mux(UPDATE_IR, SHIFT_IR) }
            is(UPDATE_IR)        { tapState := io.tms.mux(SELECT_DR, RUN_TEST_IDLE) }
        }

        // ---- IR register (5-bit) ----
        val IR_IDCODE = B"00001"
        val IR_DTMCS  = B"10000"
        val IR_DMI    = B"10001"
        val IR_BYPASS = B"11111"

        val irShift = Reg(Bits(5 bits)) init(IR_IDCODE)
        val irValue = Reg(Bits(5 bits)) init(IR_IDCODE)

        when(tapState === CAPTURE_IR) {
            irShift := B"00001"   // Capture: b0=1 per JTAG spec
        } elsewhen(tapState === SHIFT_IR) {
            irShift := io.tdi ## irShift(4 downto 1)
        }
        when(tapState === UPDATE_IR) {
            irValue := irShift
        }
        when(tapState === TEST_LOGIC_RESET) {
            irValue := IR_IDCODE
        }

        // ---- DR shift registers ----
        // IDCODE (32-bit, read-only)
        val idcodeShift = Reg(Bits(32 bits)) init(B(idcodeValue, 32 bits))

        // DTMCS (32-bit)
        // Bits: [31:18]=0, [17]=dmihardreset, [16]=dmireset, [15:12]=idle(=1),
        //       [11:10]=dmistat(=0), [9:4]=abits(=7), [3:0]=version(=1)
        val dtmcsShift = Reg(Bits(32 bits)) init(0)
        val dtmcsCapture = Bits(32 bits)
        dtmcsCapture := 0
        dtmcsCapture(3 downto 0)  := B"0001"   // version = 1
        dtmcsCapture(9 downto 4)  := B"000111"  // abits = 7
        dtmcsCapture(15 downto 12):= B"0001"    // idle = 1

        // DMI register (41-bit: addr[6:0] | data[31:0] | op[1:0])
        val dmiShift = Reg(Bits(41 bits)) init(0)

        // DMI transaction request (TCK domain)
        val dmiReqValid  = RegInit(False)
        val dmiReqAddr   = Reg(UInt(7 bits)) init(0)
        val dmiReqData   = Reg(Bits(32 bits)) init(0)
        val dmiReqOp     = Reg(Bits(2 bits)) init(0)
        val dmiRespData  = Reg(Bits(32 bits)) init(0)
        val dmiRespReady = RegInit(False)   // response available from system clock domain
        val dmiBusy      = RegInit(False)

        // BYPASS (1-bit)
        val bypassShift = Reg(Bool()) init(False)

        // ---- DR capture/shift/update ----
        when(tapState === CAPTURE_DR) {
            switch(irValue) {
                is(IR_IDCODE) { idcodeShift := B(idcodeValue, 32 bits) }
                is(IR_DTMCS)  { dtmcsShift  := dtmcsCapture }
                is(IR_DMI) {
                    // Capture: addr=0, data=response, op=status
                    val op_status = dmiBusy ? B"11" | B"00"   // 3=busy, 0=success
                    dmiShift := B(0, 7 bits) ## dmiRespData ## op_status
                }
                is(IR_BYPASS) { bypassShift := False }
            }
        }

        when(tapState === SHIFT_DR) {
            switch(irValue) {
                is(IR_IDCODE) { idcodeShift := io.tdi ## idcodeShift(31 downto 1) }
                is(IR_DTMCS)  { dtmcsShift  := io.tdi ## dtmcsShift(31 downto 1) }
                is(IR_DMI)    { dmiShift     := io.tdi ## dmiShift(40 downto 1) }
                is(IR_BYPASS) { bypassShift  := io.tdi }
            }
        }

        when(tapState === UPDATE_DR) {
            switch(irValue) {
                is(IR_DTMCS) {
                    // dmihardreset or dmireset: clear any pending DMI state
                    when(dtmcsShift(17) || dtmcsShift(16)) {
                        dmiBusy     := False
                        dmiReqValid := False
                    }
                }
                is(IR_DMI) {
                    // Extract fields from shift register
                    val op   = dmiShift(1 downto 0)
                    val data = dmiShift(33 downto 2)
                    val addr = dmiShift(40 downto 34).asUInt

                    when(!dmiBusy && (op === B"01" || op === B"10")) {
                        // Issue DMI request
                        dmiReqValid := True
                        dmiReqAddr  := addr
                        dmiReqData  := data
                        dmiReqOp    := op
                        dmiBusy     := True
                    }
                }
            }
        }

        // Clear dmiBusy when response is ready (set from system clock domain via CDC)
        when(dmiRespReady) {
            dmiBusy      := False
            dmiReqValid  := False
            dmiRespReady := False
        }

        // ---- TDO output (directly from shift registers, active during SHIFT states) ----
        val tdoData = Bool()
        tdoData := False
        when(tapState === SHIFT_IR) {
            tdoData := irShift(0)
        } elsewhen(tapState === SHIFT_DR) {
            switch(irValue) {
                is(IR_IDCODE) { tdoData := idcodeShift(0) }
                is(IR_DTMCS)  { tdoData := dtmcsShift(0) }
                is(IR_DMI)    { tdoData := dmiShift(0) }
                is(IR_BYPASS) { tdoData := bypassShift }
            }
        }
    }

    // TDO output is updated on falling edge of TCK (per JTAG spec)
    val tckFallingDomain = ClockDomain(
        clock = io.tck,
        reset = io.trst_n,
        config = ClockDomainConfig(
            clockEdge = FALLING,
            resetKind = ASYNC,
            resetActiveLevel = LOW
        )
    )
    val tdoArea = new ClockingArea(tckFallingDomain) {
        val tdoReg = RegNext(tckArea.tdoData) init(False)
    }
    io.tdo := tdoArea.tdoReg

    // ---- CDC: TCK domain -> System clock domain ----
    // Handshake-based CDC for DMI transactions.
    // TCK side sets dmiReqValid; system side detects it, performs the
    // DMI bus transaction, captures the response, and signals back.

    // Sync dmiReqValid into system clock domain (2-FF synchronizer)
    val reqValidSync = BufferCC(tckArea.dmiReqValid, init = False)
    val reqValidPrev = RegNext(reqValidSync) init(False)
    val reqRisingEdge = reqValidSync && !reqValidPrev

    // DMI bus driving (system clock domain)
    val dmiActive = RegInit(False)
    val dmiDone   = RegInit(False)

    io.dmi.req_valid := False
    io.dmi.req_addr  := BufferCC(tckArea.dmiReqAddr)
    io.dmi.req_data  := BufferCC(tckArea.dmiReqData)
    io.dmi.req_op    := BufferCC(tckArea.dmiReqOp)

    when(reqRisingEdge && !dmiActive) {
        dmiActive := True
    }

    when(dmiActive) {
        io.dmi.req_valid := True
        // After one cycle with req_valid, the DebugModule latches the response
        // (resp_valid comes one cycle later). We wait for it.
        when(io.dmi.resp_valid) {
            dmiActive := False
            dmiDone   := True
        }
    }

    // Capture response and signal back to TCK domain
    val respDataCapture = Reg(Bits(32 bits)) init(0)
    when(io.dmi.resp_valid && dmiActive) {
        respDataCapture := io.dmi.resp_data
    }

    // When reqValid goes low (TCK side acknowledged), clear dmiDone on sys side
    when(dmiDone && !reqValidSync) {
        dmiDone := False
    }

    // TCK domain: detect dmiDone rising edge -> latch response
    val tckDoneArea = new ClockingArea(tckDomain) {
        val dmiDoneTck = BufferCC(dmiDone, init = False)
        val dmiDonePrev = RegNext(dmiDoneTck) init(False)
        when(dmiDoneTck && !dmiDonePrev) {
            tckArea.dmiRespData  := BufferCC(respDataCapture)
            tckArea.dmiRespReady := True
        }
    }
}


class DebugModule extends Component {
    val io = new Bundle {
        // DMI bus (from DTM or testbench)
        val dmi = slave(DmiBus())

        // Core debug port
        val core = new Bundle {
            val halt_req   = out Bool()
            val resume_req = out Bool()
            val halted     = in Bool()
            val reg_addr   = out UInt(5 bits)
            val reg_wdata  = out Bits(32 bits)
            val reg_rdata  = in Bits(32 bits)
            val reg_wr     = out Bool()
            val csr_addr   = out UInt(12 bits)
            val csr_wdata  = out Bits(32 bits)
            val csr_rdata  = in Bits(32 bits)
            val csr_wr     = out Bool()
            val dbg_exec_req   = out Bool()
            val dbg_exec_instr = out Bits(32 bits)
            val dbg_exec_done  = in Bool()
            val dbg_exec_err   = in Bool()
        }
    }

    // DMI address constants
    val DMI_DATA0      = U(0x04, 7 bits)
    val DMI_DMCONTROL  = U(0x10, 7 bits)
    val DMI_DMSTATUS   = U(0x11, 7 bits)
    val DMI_HARTINFO   = U(0x12, 7 bits)
    val DMI_ABSTRACTCS = U(0x16, 7 bits)
    val DMI_COMMAND    = U(0x17, 7 bits)
    val DMI_ABSTRACTAUTO = U(0x18, 7 bits)
    val DMI_PROGBUF0   = U(0x20, 7 bits)
    val DMI_PROGBUF1   = U(0x21, 7 bits)
    val DMI_PROGBUF2   = U(0x22, 7 bits)
    val DMI_PROGBUF3   = U(0x23, 7 bits)

    // ---- DM registers ----
    val data0      = Reg(Bits(32 bits)) init(0)
    val dmactive   = RegInit(False)
    val haltreq    = RegInit(False)
    val cmderr     = Reg(UInt(3 bits)) init(0)
    val abstractauto = Reg(Bits(32 bits)) init(0)
    // progbuf[0..3] : 4 registres 32 bits (progbufsize=4)
    val progbuf      = Vec.fill(4)(Reg(Bits(32 bits)))
    progbuf.foreach(_ init(0))
    val cmdBusy      = RegInit(False)
    val execPhase    = Reg(UInt(2 bits)) init(0)   // 0..3
    val execWaitDone = RegInit(False)

    // Saved last command for abstractauto re-execution
    val savedCommand = Reg(Bits(32 bits)) init(0)
    val autoexecTrigger = RegInit(False)
    autoexecTrigger := False   // single-cycle pulse, set below

    // Resume request: single-cycle pulse
    val resumeReq  = RegInit(False)
    resumeReq := False   // auto-clear each cycle

    // Track dmcontrol.resumereq persistently for resumeack handshake
    val dmcontrolResumereq = RegInit(False)
    val resumeAck = RegInit(False)
    when(dmcontrolResumereq && !io.core.halted) {
        resumeAck := True
    }
    when(!dmcontrolResumereq) {
        resumeAck := False
    }

    // Drive core signals
    io.core.halt_req   := haltreq
    io.core.resume_req := resumeReq

    // Default: no register/CSR access
    io.core.reg_addr   := 0
    io.core.reg_wdata  := 0
    io.core.reg_wr     := False
    io.core.csr_addr   := 0
    io.core.csr_wdata  := 0
    io.core.csr_wr     := False
    io.core.dbg_exec_req   := False
    io.core.dbg_exec_instr := 0

    // ---- dmstatus (read-only, constructed from core signals) ----
    val dmstatus = Bits(32 bits)
    dmstatus := 0
    dmstatus(3 downto 0) := B"0010"            // version = 2 (0.13)
    dmstatus(9)          := io.core.halted      // allhalted
    dmstatus(8)          := io.core.halted      // anyhalted
    dmstatus(11)         := !io.core.halted     // allrunning  (spec 0.13 bit 11)
    dmstatus(10)         := !io.core.halted     // anyrunning  (spec 0.13 bit 10)
    dmstatus(7)          := True                // authenticated (always)
    dmstatus(17)         := resumeAck           // allresumeack (spec 0.13 bit 17)
    dmstatus(16)         := resumeAck           // anyresumeack (spec 0.13 bit 16)

    // ---- abstractcs (partially read-only) ----
    val abstractcs = Bits(32 bits)
    abstractcs := 0
    abstractcs(3 downto 0)  := B"0001"         // datacount = 1
    abstractcs(12)          := cmdBusy
    abstractcs(28 downto 24):= B"00100"        // progbufsize = 4
    abstractcs(10 downto 8) := cmderr.asBits    // cmderr

    val hartinfo = Bits(32 bits)
    hartinfo := 0

    // ---- DMI read handling ----
    // Combinational read mux, output registered for clean timing
    val resp_data_comb = Bits(32 bits)
    resp_data_comb := 0

    // Gate reads/writes to fire only on the FIRST cycle of req_valid.
    // The APB bridge (and standalone DTM) hold req_valid high for 2 system
    // clock cycles.  Without this guard the DM would process every write
    // twice — harmless for plain registers but fatal for the command
    // register: the first cycle sets cmdBusy (postexec), the second cycle
    // sees cmdBusy=True and raises cmderr=busy.
    val dmi_req_prev = RegNext(io.dmi.req_valid, init = False)
    val dmi_is_read  = io.dmi.req_valid && !dmi_req_prev && io.dmi.req_op === B"01"
    val dmi_is_write = io.dmi.req_valid && !dmi_req_prev && io.dmi.req_op === B"10"

    when(dmi_is_read) {
        switch(io.dmi.req_addr) {
            is(DMI_DATA0)      { resp_data_comb := data0 }
            is(DMI_DMCONTROL)  { resp_data_comb := B(0, 31 bits) ## dmactive.asBits }
            is(DMI_DMSTATUS)   { resp_data_comb := dmstatus }
            is(DMI_HARTINFO)   { resp_data_comb := hartinfo }
            is(DMI_ABSTRACTCS) { resp_data_comb := abstractcs }
            is(DMI_ABSTRACTAUTO) { resp_data_comb := abstractauto }
            is(DMI_PROGBUF0)   { resp_data_comb := progbuf(0) }
            is(DMI_PROGBUF1)   { resp_data_comb := progbuf(1) }
            is(DMI_PROGBUF2)   { resp_data_comb := progbuf(2) }
            is(DMI_PROGBUF3)   { resp_data_comb := progbuf(3) }
        }
    }

    io.dmi.resp_valid := dmi_req_prev
    io.dmi.resp_data  := RegNext(resp_data_comb)   init(0)

    // ---- DMI write handling ----
    when(dmi_is_write) {
        switch(io.dmi.req_addr) {
            is(DMI_DATA0) {
                data0 := io.dmi.req_data
                // abstractauto: autoexecdata[0] triggers re-execution
                when(abstractauto(0) && !cmdBusy && cmderr === 0) {
                    autoexecTrigger := True
                }
            }
            is(DMI_DMCONTROL) {
                dmactive := io.dmi.req_data(0)
                haltreq  := io.dmi.req_data(31)
                dmcontrolResumereq := io.dmi.req_data(30)
                when(io.dmi.req_data(30)) {
                    resumeReq := True
                }
            }
            is(DMI_ABSTRACTCS) {
                // Write-1-to-clear cmderr
                when(io.dmi.req_data(10 downto 8).asUInt =/= 0) {
                    cmderr := 0
                }
            }
            is(DMI_ABSTRACTAUTO) {
                abstractauto := io.dmi.req_data
            }
            is(DMI_PROGBUF0) { progbuf(0) := io.dmi.req_data }
            is(DMI_PROGBUF1) { progbuf(1) := io.dmi.req_data }
            is(DMI_PROGBUF2) { progbuf(2) := io.dmi.req_data }
            is(DMI_PROGBUF3) { progbuf(3) := io.dmi.req_data }
            is(DMI_COMMAND) {
                savedCommand := io.dmi.req_data
            }
        }
    }

    // ---- Abstract command execution (shared between DMI_COMMAND write and autoexec) ----
    val cmdExec    = Bool()
    val cmdData    = Bits(32 bits)
    val dmiCmdWrite = dmi_is_write && io.dmi.req_addr === DMI_COMMAND
    cmdExec := dmiCmdWrite || autoexecTrigger
    cmdData := dmiCmdWrite ? io.dmi.req_data | savedCommand

    when(cmdExec) {
        val cmdtype  = cmdData(31 downto 24)
        val aarsize  = cmdData(22 downto 20)
        val postexec = cmdData(18)
        val transfer = cmdData(17)
        val write    = cmdData(16)
        val regno    = cmdData(15 downto 0).asUInt
        val commandError = Bool()
        commandError := False

        // Save command for autoexec re-use
        when(dmiCmdWrite) {
            savedCommand := io.dmi.req_data
        }

        when(cmderr =/= 0) {
            commandError := True
        } elsewhen(cmdBusy) {
            cmderr := 1   // busy
            commandError := True
        } elsewhen(cmdtype =/= 0) {
            cmderr := 2   // not supported
            commandError := True
        } elsewhen(!io.core.halted) {
            cmderr := 4   // halt/resume (core not halted)
            commandError := True
        } elsewhen(transfer) {
            when(aarsize =/= B"010") {
                cmderr := 2   // only 32-bit supported
                commandError := True
            } elsewhen(regno >= 0x1000 && regno <= 0x101F) {
                io.core.reg_addr := (regno - 0x1000).resize(5)
                when(write) {
                    io.core.reg_wdata := data0
                    io.core.reg_wr    := True
                } otherwise {
                    data0 := io.core.reg_rdata
                }
            } elsewhen(regno < 0x1000) {
                io.core.csr_addr := regno.resize(12)
                when(write) {
                    io.core.csr_wdata := data0
                    io.core.csr_wr    := True
                } otherwise {
                    data0 := io.core.csr_rdata
                }
            } otherwise {
                cmderr := 2   // not supported
                commandError := True
            }
        }
        when(postexec && !commandError) {
            cmdBusy      := True
            execPhase    := 0
            execWaitDone := False
        }
    }
    // Dummy switch closure removed — logic is now outside the DMI write switch

    // Exécution séquentielle progbuf[0..3] : on s'arrête dès qu'on atteint ebreak (0x00100073) ou la fin du tableau
    when(cmdBusy) {
        when(!execWaitDone) {
            io.core.dbg_exec_req   := True
            io.core.dbg_exec_instr := progbuf(execPhase)
            execWaitDone := True
        }

        when(execWaitDone && io.core.dbg_exec_done) {
            when(io.core.dbg_exec_err) {
                cmderr := 3
                cmdBusy := False
                execWaitDone := False
            } elsewhen(progbuf(execPhase) === B(BigInt("00100073", 16), 32 bits)) {
                // ebreak atteint : fin d'exécution
                cmdBusy := False
                execWaitDone := False
            } elsewhen(execPhase =/= U(3, 2 bits)) {
                execPhase    := execPhase + 1
                execWaitDone := False
            } otherwise {
                cmdBusy := False
                execWaitDone := False
            }
        }
    }

    when(!dmactive) {
        cmdBusy := False
        execPhase := 0
        execWaitDone := False
    }
}

object DebugModuleVerilog extends App {
    val config = SpinalConfig(
        targetDirectory = "hw/gen",
        defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW)
    )
    config.generateVerilog(new DebugModule).printPruned()
}

// ============================================================
// APB → DmiBus bridge
//
// Adapts the APB master interface driven by hazard3_xilinx7_jtag_dtm
// to the DebugModule's DmiBus slave interface.
//
// The DebugModule always responds in exactly 1 cycle
//   (resp_valid = RegNext(req_valid))
// so this bridge introduces exactly 1 APB wait state, which is
// perfectly legal and expected.
//
// Address mapping:
//   xilinx7_jtag_dtm drives paddr[8:2] with the 7-bit DMI word address
//   and ties paddr[1:0] = 0, so: req_addr = paddr >> 2
// ============================================================
class ApbDmiBridge extends Component {
    val io = new Bundle {
        // APB slave side — driven by xilinx7_jtag_dtm
        val psel    = in  Bool()
        val penable = in  Bool()
        val pwrite  = in  Bool()
        val paddr   = in  UInt(9 bits)   // byte address; [8:2] = DMI word address
        val pwdata  = in  Bits(32 bits)
        val prdata  = out Bits(32 bits)
        val pready  = out Bool()
        val pslverr = out Bool()

        // DmiBus master — connected to DebugModule
        val dmi = master(DmiBus())
    }

    // Issue DMI request during APB ACCESS phase (PSEL=1 & PENABLE=1)
    io.dmi.req_valid := io.psel && io.penable
    io.dmi.req_addr  := io.paddr(8 downto 2)
    io.dmi.req_data  := io.pwdata
    io.dmi.req_op    := io.pwrite ? B"10" | B"01"

    // DebugModule responds exactly 1 cycle after req_valid → becomes PREADY
    io.prdata  := io.dmi.resp_data
    io.pready  := io.dmi.resp_valid
    io.pslverr := False
}

// ============================================================
// BlackBox wrapper for hazard3_xilinx7_jtag_dtm
//
// Uses Xilinx 7-series BSCANE2 primitives to implement the JTAG TAP.
// No external JTAG pins are required — JTAG access goes through the
// FPGA's built-in JTAG chain (USER3/USER4 scan chains by default).
//
// Connect clk_dmi / rst_n_dmi to the system clock domain.
// Connect the dmi_* APB ports to an ApbDmiBridge to reach DebugModule.
// ============================================================
class Xilinx7JtagDtm(
    abits    : Int = 7,
    wPaddr   : Int = 9,
    idleHint : Int = 7
) extends BlackBox {
    val io = new Bundle {
        val dmihardreset_req = out Bool()
        val clk_dmi          = in  Bool()
        val rst_n_dmi        = in  Bool()

        // APB master interface — drive into ApbDmiBridge
        val dmi_psel    = out Bool()
        val dmi_penable = out Bool()
        val dmi_pwrite  = out Bool()
        val dmi_paddr   = out UInt(wPaddr bits)
        val dmi_pwdata  = out Bits(32 bits)
        val dmi_prdata  = in  Bits(32 bits)
        val dmi_pready  = in  Bool()
        val dmi_pslverr = in  Bool()
    }

    addGeneric("ABITS",           abits)
    addGeneric("W_PADDR",         wPaddr)
    addGeneric("DTMCS_IDLE_HINT", idleHint)
    setDefinitionName("xilinx7_jtag_dtm")
    noIoPrefix()
}
