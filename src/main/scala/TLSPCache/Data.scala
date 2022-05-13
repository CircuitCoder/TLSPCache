package TLSPCache

import chisel3._

class Addr(implicit val params: Params[Data]) extends Bundle {
  val tag = UInt(params.tagWidth.W)
  val idx = UInt(params.idxWidth.W)
  val bnkidx = if(params.bnkidxWidth > 0) UInt(params.bnkidxWidth.W) else null
  val offset = UInt(params.bnkidxWidth.W)
  val zero = UInt(params.zeroWidth.W)
}

/**
  * Request sent in cycle 0
  */
class PreflightRequest[Bypass <: Data](implicit val params: Params[Bypass]) extends Bundle {
  val bypass = params.bypassGen()
  val vaddr = UInt(params.addrWidth.W)
}

/**
  * Reqeust following on cycle 1
  */
class MainRequest(implicit val params: Params[Data]) extends Bundle {
  val paddr = UInt(params.addrWidth.W)
  val we = Vec(params.writePerAccess, Bool())
  val wdata = UInt((params.accessSize * 8).W)

  def wdataAsWrites = wdata.asTypeOf(Vec(params.writePerAccess, UInt((params.writeGranularity * 8).W)))
}

class Response[Bypass <: Data](implicit val params: Params[Bypass]) extends Bundle {
  val bypass = params.bypassGen()
  val data = UInt((params.accessSize * 8).W)

  // Bus error!
  val error = Bool()
}