package TLSPCache

import chisel3.stage.ChiselStage
import chisel3._

object Main extends App {
  (new ChiselStage).emitSystemVerilog(
    new TLSPCache.Cache[UInt]()(
      new {
        override val accessSize: Int = 8
        override val bypassGen: () => UInt = () => { UInt(64.W) }
      } with Params[UInt] 
    ),
    args
  )
}