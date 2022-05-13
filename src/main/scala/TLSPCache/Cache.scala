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
  val ReleaseReq = Value
  val ReleaseResp = Value
  val AcquireReq = Value
  val AcquireResp = Value
  val AcquireGrant = Value
}

class MSHR[Bypass <: Data](implicit val params: Params[Bypass]) extends Module {
  val alloc = IO(Flipped(Decoupled(new MSHRAllocRequest[Bypass])))
  val data = IO(Decoupled(new DataAccessRequest))
  val out = IO(new TLDecoupledBundle(params.tlParam))

  val state = RegInit(MSHRState.Empty)
  val req = Reg(new MSHRAllocRequest[Bypass])

  def empty = state === MSHRState.Empty

  // Allocation
  alloc.ready := empty
  when(alloc.fire) {
    req := alloc.bits
    state := Mux(alloc.bits.hasRelease, MSHRState.ReleaseReq, MSHRState.AcquireReq)
  }
}

object ArbiteredRequestType extends ChiselEnum {
  val Master = Bool
  val SlaveProbe = Value
}

class ArbiteredRequest[Bypass <: Data](implicit val params: Params[Bypass]) {
  val addr = UInt(params.addrWidth.W)
  // Additionally, contains operations from L2
  val ty = ArbiteredRequestType()
  val toCap = CapParam()
  // Is this a probeBlock? Used to assert not modified
  val debugWithBlock = Bool()
}

class MSHRAllocRequest[Bypass <: Data](implicit val params: Params[Bypass]) extends Bundle {
  val req = new MainRequest
  val assoc = UInt(log2Up(params.assocCnt).W)

  val hasRelease = Bool()
  val releaseCap = PruneReportParam()
  val releaseTag = UInt(params.tagWidth.W)
  def hasReleaseData = releaseCap === PruneReportParam.TtoN

  val hasAcquire = Bool()
  val acquireCap = GrowParam()
  def hasAcquireData = acquireCap =/= GrowParam.BtoT
}

class DataAccessRequest(implicit val params: Params[Data]) extends Bundle {
  val idx = UInt(params.idxWidth.W)
  val offset = UInt(params.offsetWidth.W)
  val assoc = UInt(log2Up(params.assocCnt).W)

  val we = Vec(params.writePerAccess, Bool())
  val wdata = UInt(params.accessSize.W)
}

class Cache[Bypass <: Data](implicit val params: Params[Bypass]) extends Module {
  /**
    * Ports
    */
  val preflight = IO(Flipped(Decoupled(new PreflightRequest[Bypass])))
  val main = IO(Input(new MainRequest))

  val resp = IO(Valid(new Response[Bypass]))

  val out = IO(new tilelink.TLDecoupledBundle(params.tlParam))

  /**
   * Storages
   */
  val banks = for(i <- 0 until params.bankCnt) yield new Bank(s"Bank $i")
  val mshrs = for(i <- 0 until params.missesUnderHit) yield Module(new MSHR).suggestName(s"MSHR $i")

  val bank_data_reqs = for(i <- 0 until params.bankCnt) yield new {
    val pipeline = Wire(Decoupled(new DataAccessRequest))
    val mshr = Wire(Decoupled(new DataAccessRequest))
  }
  val bank_data_resp = for(i <- 0 until params.bankCnt) yield Wire(UInt((params.accessSize * 8).W))

  val mshr_allocs = for(i <- 0 until params.bankCnt) yield Wire(Decoupled(new MSHRAllocRequest[Bypass]))

  /**
    * Helpers
    */
  val victim_gen = chisel3.util.random.GaloisLFSR.maxPeriod(log2Up(params.assocCnt))

  /**
    * Stage control signals
    */
  val s0_flow = Wire(Bool())
  val s1_flow = Wire(Bool())
  val s2_flow = Wire(Bool())

  val s0_valid = Wire(Bool())
  val s1_valid = RegEnable(s0_valid && s0_flow, s1_flow, false.B)
  val s2_valid = RegEnable(s1_valid && s1_flow, s2_flow, false.B)

  /**
    * Stage 0
    * 
    * Fire request into metadata file
    */
  val s0_addr = preflight.bits.vaddr.asTypeOf(new Addr)
  val s0_idx = s0_addr.idx

  val s0_valids = banks(0).validBits(s0_idx)
  s0_valid := preflight.ready
  s0_flow := s1_flow || !s1_valid
  preflight.ready := s0_flow

  /**
    * Stage 1
    */
  val s1_addr = main.paddr.asTypeOf(new Addr)
  val s1_tags_readout = banks(0).tags.read(s0_idx)
  val s1_tags = Wire(s1_tags_readout.cloneType)
  s1_tags := Mux(RegNext(s1_flow), s1_tags_readout, RegNext(s1_tags))

  val s1_bypass = RegEnable(preflight.bits.bypass, s1_flow)
  val s1_idx = RegEnable(s0_idx, s1_flow)
  assert(s1_idx === s1_addr.idx)
  val s1_valids = RegEnable(s0_valids, s1_flow)
  val s1_hitmap = s1_tags.map(_ === s1_addr.tag).zip(s1_valids).map({ case (l, r) => l && r })
  val s1_hit_assoc = OHToUInt(s1_hitmap)
  val s1_hit = VecInit(s1_hitmap).asUInt.orR
  val s1_full = VecInit(s1_hitmap).asUInt.andR
  val debug_s1_hit_cnt = PopCount(s1_hitmap)
  assert(!s1_valid || debug_s1_hit_cnt <= 1.U)

  // Victim or hit assoc
  val s1_victim = Mux(s1_full, victim_gen, PriorityEncoder(VecInit(s1_hitmap.map(!_)).asUInt))
  val s1_victim_tag = s1_tags(victim_gen)
  val s1_victim_dirty = banks(0).dirtyBits(s1_idx)(victim_gen)

  val s1_data_idx = s1_addr.idx ## s1_addr.offset ## s1_hit_assoc
  // Read / write port for data
  val s1_has_write = main.we.asUInt.orR

  // TODO: before write, check permission
  // TODO: use a arbiter to arbiter between MSHR read
  when(s1_has_write && s1_valid && s1_flow && s1_hit) {
    banks(0).data.write(s1_data_idx, main.wdataAsWrites, main.we)
  }
  // Override dirty bits
  for(i <- 0 until params.assocCnt) {
    banks(0).dirtyBits(s1_idx)(i) := s1_valid && s1_flow && s1_hitmap(i) && s1_has_write || banks(0).dirtyBits(s1_idx)(i)
  }

  s1_flow := s2_flow || !s2_valid

  /**
    * Stage 2
    */

  // If not hit, readout is undefined. If hit, readout is the result data
  val s2_data_readout = banks(0).data.read(s1_data_idx)
  val s2_data = Wire(s2_data_readout.cloneType)
  s2_data := Mux(RegNext(s2_flow), s2_data_readout, RegNext(s2_data))

  val s2_bypass = RegEnable(s1_bypass, s2_flow)
  val s2_main = RegEnable(main, s2_flow)
  val s2_hit = RegEnable(s1_hit, s2_flow)
  val s2_hit_assoc = RegEnable(s1_hit_assoc, s2_flow)
  val s2_victim = RegEnable(s1_victim, s2_flow)
  val s2_victim_tag = RegEnable(s1_victim_tag, s2_flow)
  val s2_victim_dirty = RegEnable(s1_victim_dirty, s2_flow)
  val s2_full = RegEnable(s1_full, s2_flow)
  val s2_has_write = RegEnable(s1_has_write, s2_flow)

  resp.bits.bypass := s2_bypass
  resp.bits.data := s2_data.asUInt
  resp.bits.error := false.B
  // TODO: check if is request from client
  resp.valid := s2_hit

  // TODO: push to replay queue if misses but has MSHR with same idx
  mshr_allocs(0).bits.req := s2_main
  mshr_allocs(0).bits.hasRelease := s2_full
  mshr_allocs(0).bits.releaseCap := Mux(s2_victim_dirty, PruneReportParam.TtoN, PruneReportParam.BtoN)
  mshr_allocs(0).bits.releaseTag := s2_victim_tag
  mshr_allocs(0).bits.hasAcquire := true.B // TODO: Implements non-client requests!
  mshr_allocs(0).bits.acquireCap := Mux(s2_has_write, GrowParam.NtoT, GrowParam.NtoB) // TODO: implements write with non-sufficient permission
  
  mshr_allocs(0).valid := !s2_hit

  s2_flow := s2_hit || mshr_allocs(0).ready

  /**
    * MSHR managements
    */
  val mshr_alloc_arb = Module(new RRArbiter(new MSHRAllocRequest[Bypass], params.bankCnt))
  mshr_alloc_arb.io.in.zip(mshr_allocs).foreach({ case (l, r) => l <> r })
  var mshr_alloced = false.B
  for(mshr <- mshrs) {
    mshr.alloc.bits := mshr_alloc_arb.io.out.bits
    mshr.alloc.valid := mshr_alloc_arb.io.out.valid && !mshr_alloced
    mshr_alloced = mshr_alloced || !mshr.alloc.ready
  }

  mshr_alloc_arb.io.out.ready := mshr_alloced
  
  /**
    * MSHR <-> TL
    */
}