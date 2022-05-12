package TLSPCache

import chisel3._
import chisel3.util._

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

  require(assocSize % lineSize == 0)
  require(lineSize % accessSize == 0)
  require(accessSize % writeGranularity == 0)
  require(bankCnt == 1, "Sorry, not implemented (yet)")

  def linePerAssoc: Int = assocSize / lineSize
  def accessPerLine: Int = lineSize / accessSize
  def writePerAccess: Int = accessSize / writeGranularity

  def bnkidxWidth: Int = log2Up(bankCnt)
  def idxWidth: Int = log2Up(linePerAssoc) - bnkidxWidth
  def offsetWidth: Int = log2Up(accessPerLine)
  def zeroWidth: Int = log2Up(accessSize)
  def tagWidth: Int = addrWidth - idxWidth - idxWidth - bnkidxWidth - zeroWidth
}