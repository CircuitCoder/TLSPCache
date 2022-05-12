// import Mill dependency
import mill._
import mill.define.Sources
import mill.modules.Util
import mill.scalalib.TestModule.ScalaTest
import scalalib._
// support BSP
import mill.bsp._
import publish._

import $file.deps.TileLink.build

import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version_mill0.9:0.1.1`
import de.tobiasroeser.mill.vcs.version.VcsVersion

object TileLink extends Cross[TileLink]("2.12.13")
class TileLink(override val crossScalaVersion: String) extends deps.TileLink.build.tilelink(crossScalaVersion) {
  override def millSourcePath = os.pwd / "deps" / "TileLink"

  override def sources = T.sources { os.pwd / "deps" / "TileLink" / "tilelink" }
}

object TLSPCache extends Cross[TLSPCache]("2.12.13")
class TLSPCache(val crossScalaVersion: String) extends CrossScalaModule {
  override def millSourcePath = os.pwd
  override def scalacOptions = Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
    "-P:chiselplugin:genBundleElements"
  )
  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.5.3",
  )
  override def scalacPluginIvyDeps = Agg(
    ivy"edu.berkeley.cs:::chisel3-plugin:3.5.3",
  )
  override def moduleDeps = super.moduleDeps :+ TileLink(crossScalaVersion)
}