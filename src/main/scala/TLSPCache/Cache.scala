package TLSPCache

import chisel3._
import chisel3.util._
import chisel3.experimental._
import tilelink._

class Bank(name: String)(implicit val params: Params[Data]) {
  val validBits = RegInit(VecInit.fill(params.linePerAssoc, params.assocCnt)(false.B)).suggestName(s"$name.validBits")
  val dirtyBits = RegInit(VecInit.fill(params.linePerAssoc, params.assocCnt)(false.B)).suggestName(s"$name.dirtyBits")
  // TODO: do we need exclusive bits?
  val tags = SyncReadMem(params.linePerAssoc, Vec(params.assocCnt, UInt(params.tagWidth.W))).suggestName(s"$name.tags")
  val data = SyncReadMem(
    params.linePerAssoc * params.accessPerLine * params.assocCnt,
    Vec(params.writePerAccess, UInt((params.writeGranularity * 8).W))
  ).suggestName(s"$name.tags")
}

object MSHRState extends ChiselEnum {
  val Empty = Value
  val Request = Value
  val Response = Value
}

class MSHR[Bypass <: Data](implicit val params: Params[Bypass]) extends Bundle {
  val state = MSHRState
  val req = new MainRequest
}

object ArbiteredRequestType extends ChiselEnum {
  val Master = Bool
  val SlaveProbe = Value
}

class ArbiteredRequest[Bypass <: Data](implicit override val params: Params[Bypass]) extends PreflightRequest[Bypass] {
  // Additionally, contains operations from L2
  val ty = ArbiteredRequestType()
  val toCap = CapParam()
  // Is this a probeBlock? Used to assert not modified
  val debugWithBlock = Bool()
}

class Cache[Bypass <: Data](implicit val params: Params[Bypass]) extends Module {
  /**
    * Ports
    */
  val preflight = IO(Flipped(Decoupled(new PreflightRequest[Bypass])))
  val main = IO(Input(new MainRequest))

  val resp = IO(Valid(new Response[Bypass]))

  val banks = for(i <- 0 until params.bankCnt) yield new Bank(s"Bank $i")
  val mshrs = for(i <- 0 until params.missesUnderHit) yield (new MSHR).suggestName(s"MSHR $i")

  /**
    * Helpers
    */
  val victimGen = chisel3.util.random.GaloisLFSR.maxPeriod(log2Up(params.assocCnt))

  /**
    * Stage control signals
    */
  val s0_flow = Wire(Bool())
  val s1_flow = Wire(Bool())
  

  val s0_valid = Wire(Bool())
  val s1_valid = RegEnable(s0_valid && s0_flow, s1_flow, false.B)

  /**
    * Stage 0
    */
  val s0_addr = preflight.bits.vaddr.asTypeOf(new Addr)
  val s0_idx = s0_addr.idx

  val s0_valid_readout = banks(0).validBits(s0_idx)
  s0_valid := preflight.ready
  s0_flow := s1_flow || !s1_valid

  /**
    * Stage 1
    */
  val s1_addr = main.paddr.asTypeOf(new Addr)
  val s1_tags = banks(0).tags.read(s0_idx)
  val s1_idx = RegEnable(s0_idx, s1_flow)
  assert(s1_idx === s1_addr.idx)
  val s1_valid_readout = RegEnable(s0_valid_readout, s1_flow)
  val s1_hitmap = s1_tags.map(_ === s1_addr.tag).zip(s1_valid_readout).map({ case (l, r) => l && r })
  val s1_hit_assoc = OHToUInt(s1_hitmap)
  val s1_hit = VecInit(s1_hitmap).asUInt.orR
  val debug_s1_hit_cnt = PopCount(s1_hitmap)
  assert(!s1_valid || debug_s1_hit_cnt <= 1.U)

  // Victim or hit assoc
  val s1_assoc = Mux(s1_hit, s1_hit_assoc, victimGen)

  val s1_data_idx = s1_addr.idx ## s1_addr.offset ## s1_assoc
  // Read / write port for data
  val s2_data_readout = banks(0).data.read(s1_data_idx)
  val s1_has_write = main.we.asUInt.orR
  when(s1_has_write && s1_flow && s1_hit) {
    banks(0).data.write(s1_data_idx, main.wdataAsWrites, main.we)
  }
  // Override dirty bits
  for(i <- 0 until params.assocCnt) {
    banks(0).dirtyBits(s1_idx)(i) := s1_flow && s1_hitmap(i) && s1_has_write || banks(0).dirtyBits(s1_idx)(i)
  }
}