

/******************************************************************************
  * This file describes the AXI4-Lite interface
  *
  * Interface :
  *   _______________________________________________________________________
  *  | Global  | Write Addr | Write Data | Write Rsp | Read Addr | Read Data |
  *  |   -     |    aw      |     wr     |     b     |    ar     |     r     |
  *  |----------------------- -----------------------------------------------|
  *  | aclk    | awvalid    | wvalid     | bvalid    | arvalid   | rvalid    |
  *  | arstn   | awready    | wready     | bready    | arready   | rready    |
  *  |         | awaddr     | wdata      | bresp     | araddr    | rdata     |
  *  |         | awprot     | wstrb      |           | arprot    | rresp     |
  *  |_________|____________|____________|___________|___________|___________|
  *
  */


package rv.bus.axi4lite
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._

/**
  * Definition of the constants used by the AXI Lite bus
  */
object Axi4Lite {

  def apply(addressWidth : Int,
            dataWidth    : Int) = new Axi4Lite(Axi4LiteConfig(
    addressWidth = addressWidth,
    dataWidth = dataWidth
  ))

  /**
    * Read-write response
    */
  object resp {
    def apply() = Bits(2 bits)
    def OKAY   = B"00" // Normal access success
    def EXOKAY = B"01" // Exclusive access okay
    def SLVERR = B"10" // Slave error
    def DECERR = B"11" // Decode error
  }

  /**
    * Access permissions
    */
  object prot{
    def apply() = Bits(3 bits)
    def UNPRIVILEGED_ACCESS = B"000"
    def PRIVILEGED_ACCESS   = B"001"
    def SECURE_ACCESS       = B"000"
    def NON_SECURE_ACCESS   = B"010"
    def DATA_ACCESS         = B"000"
    def INSTRUCTION_ACCESS  = B"100"
  }
}


/**
  * Configuration class for the Axi Lite bus
  * @param addressWidth Width of the address bus
  * @param dataWidth    Width of the data bus
  */
case class Axi4LiteConfig(addressWidth : Int,
                          dataWidth    : Int,
                          useProt      : Boolean = false,
                          readIssuingCapability     : Int = -1,
                          writeIssuingCapability    : Int = -1,
                          combinedIssuingCapability : Int = -1,
                          readDataReorderingDepth   : Int = -1){
  def bytePerWord = dataWidth/8
  def addressType = UInt(addressWidth bits)
  def dataType = Bits(dataWidth bits)

  require(dataWidth == 32 || dataWidth == 64, "Data width must be 32 or 64")
}


/**
  * Definition of the Write/Read address channel
  * @param config Axi Lite configuration class
  */
case class Axi4LiteAx(config: Axi4LiteConfig) extends Bundle {

  val addr = UInt(config.addressWidth bits)
  val prot = if (config.useProt) Bits(3 bits) else null


  import Axi4Lite.prot._

  def setUnprivileged : Unit = prot := UNPRIVILEGED_ACCESS | SECURE_ACCESS | DATA_ACCESS
  def setPermissions ( permission : Bits ) : Unit = prot := permission
}


/**
  * Definition of the Write data channel
  * @param config Axi Lite configuration class
  */
case class Axi4LiteW(config: Axi4LiteConfig) extends Bundle {
  val data = Bits(config.dataWidth bits)
  val strb = Bits(config.dataWidth / 8 bits)

  def setStrb : Unit = strb := (1 << widthOf(strb))-1
  def setStrb(bytesLane : Bits) : Unit = strb := bytesLane
}


/**
  * Definition of the Write response channel
  * @param config Axi Lite configuration class
  */
case class Axi4LiteB(config: Axi4LiteConfig) extends Bundle {
  val resp = Bits(2 bits)

  import Axi4Lite.resp._

  def setOKAY()   : Unit = resp := OKAY
  def setEXOKAY() : Unit = resp := EXOKAY
  def setSLVERR() : Unit = resp := SLVERR
  def setDECERR() : Unit = resp := DECERR
  def isOKAY()   : Bool = resp === OKAY
  def isEXOKAY() : Bool = resp === EXOKAY
  def isSLVERR() : Bool = resp === SLVERR
  def isDECERR() : Bool = resp === DECERR
}

/** Companion object to create hard-wired AXI responses. */
object Axi4LiteB {
  def okay(config: Axi4LiteConfig) = { val resp = new Axi4LiteB(config); resp.setOKAY(); resp }
  def exclusiveOkay(config: Axi4LiteConfig) = { val resp = new Axi4LiteB(config); resp.setEXOKAY(); resp }
  def slaveError(config: Axi4LiteConfig) = { val resp = new Axi4LiteB(config); resp.setSLVERR(); resp }
  def decodeError(config: Axi4LiteConfig) = { val resp = new Axi4LiteB(config); resp.setDECERR(); resp }
}


/**
  * Definition of the Read data channel
  * @param config Axi Lite configuration class
  */
case class Axi4LiteR(config: Axi4LiteConfig) extends Bundle {
  val data = Bits(config.dataWidth bits)
  val resp = Bits(2 bits)

  import Axi4Lite.resp._

  def setOKAY()   : Unit = resp := OKAY
  def setEXOKAY() : Unit = resp := EXOKAY
  def setSLVERR() : Unit = resp := SLVERR
  def setDECERR() : Unit = resp := DECERR
  def isOKAY()   : Bool = resp === OKAY
  def isEXOKAY() : Bool = resp === EXOKAY
  def isSLVERR() : Bool = resp === SLVERR
  def isDECERR() : Bool = resp === DECERR
}


/**
  * Axi Lite interface definition
  * @param config Axi Lite configuration class
  */
case class Axi4Lite(config: Axi4LiteConfig) extends Bundle with IMasterSlave {

  val aw = Stream(Axi4LiteAx(config))
  val w  = Stream(Axi4LiteW(config))
  val b  = Stream(Axi4LiteB(config))
  val ar = Stream(Axi4LiteAx(config))
  val r  = Stream(Axi4LiteR(config))

  //Because aw w b ar r are ... very lazy
  def writeCmd  = aw
  def writeData = w
  def writeRsp  = b
  def readCmd   = ar
  def readRsp   = r


  def >> (that : Axi4Lite) : Unit = {
    assert(that.config == this.config)
    this.writeCmd  >> that.writeCmd
    this.writeData >> that.writeData
    this.writeRsp  << that.writeRsp

    this.readCmd  >> that.readCmd
    this.readRsp << that.readRsp
  }

  def <<(that : Axi4Lite) : Unit = that >> this

  def >>(that: Axi4LiteWriteOnly): Unit = {
    assert(that.config == this.config)
    this.writeCmd  >> that.writeCmd
    this.writeData >> that.writeData
    this.writeRsp  << that.writeRsp
  }

  def <<(that: Axi4LiteWriteOnly): Unit = that >> this

  def >>(that: Axi4LiteReadOnly): Unit = {
    assert(that.config == this.config)
    this.readCmd >> that.readCmd
    this.readRsp << that.readRsp
  }

  def <<(that: Axi4LiteReadOnly): Unit = that >> this

  override def asMaster(): Unit = {
    master(aw,w)
    slave(b)

    master(ar)
    slave(r)
  }

  def pipelined(
                 aw: StreamPipe = StreamPipe.NONE,
                 w: StreamPipe = StreamPipe.NONE,
                 b: StreamPipe = StreamPipe.NONE,
                 ar: StreamPipe = StreamPipe.NONE,
                 r: StreamPipe = StreamPipe.NONE
               ): Axi4Lite = {
    val ret = cloneOf(this)
    ret.aw << this.aw.pipelined(aw)
    ret.w << this.w.pipelined(w)
    ret.b.pipelined(b) >> this.b
    ret.ar << this.ar.pipelined(ar)
    ret.r.pipelined(r) >> this.r
    ret
  }
}


object  Axi4LiteSpecRenamer{
  def apply(that : Axi4Lite): Axi4Lite ={
    def doIt = {
      that.flatten.foreach((bt) => {
        bt.setName(bt.getName().replace("_payload_",""))
        bt.setName(bt.getName().replace("_valid","valid"))
        bt.setName(bt.getName().replace("_ready","ready"))
        if(bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_",""))
      })
    }
    if(Component.current == that.component)
      that.component.addPrePopTask(() => {doIt})
    else
      doIt

    that
  }
  def apply(that : Axi4LiteReadOnly): Unit ={
    def doIt = {
      that.flatten.foreach((bt) => {
        bt.setName(bt.getName().replace("_payload_",""))
        bt.setName(bt.getName().replace("_valid","valid"))
        bt.setName(bt.getName().replace("_ready","ready"))
        if(bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_",""))
      })
    }
    if(Component.current == that.component)
      that.component.addPrePopTask(() => {doIt})
    else
      doIt
  }
}

case class Axi4LiteReadOnly(config: Axi4LiteConfig) extends Bundle with IMasterSlave {
  val ar = Stream(Axi4LiteAx(config))
  val r  = Stream(Axi4LiteR(config))

  def readCmd   = ar
  def readRsp   = r

  def >>(that: Axi4Lite): Unit = {
    assert(that.config == this.config)
    this.readCmd >> that.readCmd
    this.readRsp << that.readRsp
  }

  def <<(that: Axi4Lite): Unit = that >> this

  def >>(that: Axi4LiteReadOnly): Unit = {
    assert(that.config == this.config)
    this.readCmd >> that.readCmd
    this.readRsp << that.readRsp
  }

  def <<(that: Axi4LiteReadOnly): Unit = that >> this

  override def asMaster(): Unit = {
    master(ar)
    slave(r)
  }
}


case class Axi4LiteWriteOnly(config: Axi4LiteConfig) extends Bundle with IMasterSlave {
  val aw = Stream(Axi4LiteAx(config))
  val w  = Stream(Axi4LiteW(config))
  val b  = Stream(Axi4LiteB(config))


  def writeCmd  = aw
  def writeData = w
  def writeRsp  = b

  def >>(that: Axi4Lite): Unit = {
    assert(that.config == this.config)
    this.writeCmd >> that.writeCmd
    this.writeData >> that.writeData
    this.writeRsp << that.writeRsp
  }

  def <<(that: Axi4Lite): Unit = that >> this

  def >>(that: Axi4LiteWriteOnly): Unit = {
    assert(that.config == this.config)
    this.writeCmd >> that.writeCmd
    this.writeData >> that.writeData
    this.writeRsp << that.writeRsp
  }

  def <<(that: Axi4LiteWriteOnly): Unit = that >> this

  override def asMaster(): Unit = {
    master(aw,w)
    slave(b)
  }
}


object Axi4LiteSlaveFactory {
  def apply(bus: Axi4Lite) = new Axi4LiteSlaveFactory(bus)
}

class Axi4LiteSlaveFactory(bus : Axi4Lite, useWriteStrobes : Boolean = false) extends BusSlaveFactoryDelayed{

  val readHaltRequest = False
  val writeHaltRequest = False

  val writeJoinEvent = StreamJoin.arg(bus.writeCmd,bus.writeData)
  val writeRsp = Axi4LiteB(bus.config)
  bus.writeRsp << writeJoinEvent.translateWith(writeRsp).haltWhen(writeHaltRequest).halfPipe()

  val readDataStage = bus.readCmd.halfPipe()
  val readRsp = Axi4LiteR(bus.config)
  bus.readRsp << readDataStage.haltWhen(readHaltRequest).translateWith(readRsp)

  when(writeErrorFlag) {
    writeRsp.setSLVERR()
  }otherwise {
    writeRsp.setOKAY()
  }

  when(readErrorFlag){
    readRsp.setSLVERR()
  }otherwise {
    readRsp.setOKAY()
  }

  readRsp.data := 0

  override def writeByteEnable() : Bits = useWriteStrobes generate(bus.writeData.strb)

  def maskAddress(addr : UInt) = addr & ~U(bus.config.dataWidth/8 -1, bus.config.addressWidth bits)
  val readAddressMasked  = maskAddress(readDataStage.addr)
  val writeAddressMasked = maskAddress(bus.writeCmd.addr)

  override def readAddress(): UInt  = readAddressMasked
  override def writeAddress(): UInt = writeAddressMasked

  override def readHalt(): Unit = readHaltRequest := True
  override def writeHalt(): Unit = writeHaltRequest := True

  val writeOccur = writeJoinEvent.fire
  val readOccur = bus.readRsp.fire

  override def build(): Unit = {
    super.doNonStopWrite(bus.writeData.data)

    switch(writeAddress()) {
      for ((address, jobs) <- elementsPerAddress)address match {
        case address : SingleMapping =>
          assert(address.address % (bus.config.dataWidth/8) == 0)
          is(address.address) {
            doMappedWriteElements(jobs, writeJoinEvent.valid, writeOccur, bus.writeData.data)
          }
        case _ =>
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(writeAddress())){
        doMappedWriteElements(jobs,writeJoinEvent.valid, writeOccur, bus.writeData.data)
      }
    }


    switch(readAddress()) {
      for ((address, jobs) <- elementsPerAddress) address match {
        case address : SingleMapping =>
          assert(address.address % (bus.config.dataWidth/8) == 0)
          is(address.address) {
            doMappedReadElements(jobs, readDataStage.valid, readOccur, readRsp.data)
          }
        case _ =>
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(readAddress())){
        doMappedReadElements(jobs,readDataStage.valid, readOccur, readRsp.data)
      }
    }
  }

  override def busDataWidth: Int = bus.config.dataWidth

  override def wordAddressInc: Int = busDataWidth / 8
}
