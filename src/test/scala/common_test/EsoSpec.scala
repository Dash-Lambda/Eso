package common_test

import java.io.File

import common.{Config, Interpreter}
import org.scalatest.flatspec.AnyFlatSpec
import ui.EsoRunState

import scala.io.Source
import scala.util.Try

abstract class EsoSpec extends AnyFlatSpec{
  val defaultConfig: Config = EsoRunState.default.config
  
  def grabFile(str: String, encoding: String = "UTF-8"): String = {
    val iFile = new File(s"testResources/$str")
    assume(iFile.exists, s"$str not in testResources")
    val src = Source.fromFile(iFile)
    val prog = src.mkString
    src.close()
    prog}
  
  def getOutput(intp: Interpreter, prog: String, inp: Seq[Char] = Seq(), config: Config = defaultConfig): Try[LazyList[Char]] = intp(config)(prog) map (i => i(inp))
  def getOutputString(intp: Interpreter, prog: String, inp: Seq[Char] = Seq(), config: Config = defaultConfig): Try[String] = getOutput(intp, prog, inp, config) map (_.mkString)
  
  def testInterp(intp: Interpreter, config: Config, prog: String, inp: Seq[Char] = Seq())(f: Try[LazyList[Char]] => Boolean): Boolean = f(intp(config)(prog) map (i => i(inp)))
  def outputEquals(exp: String)(lop: Try[LazyList[Char]]): Boolean = lop.map(lst => lst.mkString == exp).getOrElse(false)
}