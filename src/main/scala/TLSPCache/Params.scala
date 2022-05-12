package TLSPCache

import chisel3._
import chisel3.util._
import tilelink._

/**
  * Parameters.
  * Use bypassGen to pass anything and get it back in response (e.g. an ID)
  */
trait Params[+Bypass <: Data] {
  val addrWidth: Int = 64

  val accessSize: Int
  val writeGranularity: Int = 8

  val lineSize: Int = 64
  val assocSize: Int = 4096
  val assocCnt: Int = 4

  val bankCnt: Int = 1

  val missesUnderHit: Int = 1
  val bypassGen: () => Bypass

  // By default, use 64-bit memory interface
  val outDataSize: Int = 8
  val outSinkWidth: Int = 1

  require(assocSize % lineSize == 0)
  require(lineSize % accessSize == 0)
  require(accessSize % writeGranularity == 0)
  require(accessSize % outDataSize == 0)
  require(bankCnt == 1, "Sorry, not implemented (yet)")

  def linePerAssoc: Int = assocSize / lineSize
  def accessPerLine: Int = lineSize / accessSize
  def writePerAccess: Int = accessSize / writeGranularity

  def bnkidxWidth: Int = log2Up(bankCnt)
  def idxWidth: Int = log2Up(linePerAssoc) - bnkidxWidth
  def offsetWidth: Int = log2Up(accessPerLine)
  def zeroWidth: Int = log2Up(accessSize)
  def tagWidth: Int = addrWidth - idxWidth - idxWidth - bnkidxWidth - zeroWidth

  def tlParam: TLBundleParameter = {
    class Params {
      val dataWidth: Int = outDataSize * 8
      val addressWidth: Int = addrWidth
      val sizeWidth: Int = log2Up(lineSize)

      val sourceWidth: Int = log2Up(missesUnderHit)
      val sinkWidth: Int = outSinkWidth

      val hasCorrupt: Boolean = true
      val hasDenied: Boolean = true
    }

    val a = new {} with Params with TLChannelAParameter
    val b = new {} with Params with TLChannelBParameter
    val c = new {} with Params with TLChannelCParameter
    val d = new {} with Params with TLChannelDParameter
    val e = new {} with Params with TLChannelEParameter

    TLBundleParameter(a, Some(b), Some(c), d, Some(e))
  }
}