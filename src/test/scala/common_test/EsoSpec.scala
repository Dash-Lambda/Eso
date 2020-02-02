package common_test

import common.{Config, Interpreter}
import org.scalatest.flatspec.AnyFlatSpec
import ui.{EsoFileReader, EsoRunState}

import scala.collection.immutable
import scala.util.Try

abstract class EsoSpec extends AnyFlatSpec{
  val defaultConfig: Config = EsoRunState.default.config
  
  def grabFile(str: String, normLineBreaks: Boolean = true): String = {
    val tried = EsoFileReader.readFile(s"testResources/$str")
    assume(tried.isSuccess)
    tried.get}
  
  def mkMap[A, B](vec: Vector[(A, B)]): immutable.HashMap[A, B] = {
    val builder = immutable.HashMap.newBuilder[A, B]
    builder ++= vec
    builder.result}
  
  def getOutput(intp: Interpreter, prog: String, inp: Seq[Char] = Seq(), config: Config = defaultConfig): Try[LazyList[Char]] = intp(config)(prog) map (i => i(inp))
  def getOutputString(intp: Interpreter, prog: String, inp: Seq[Char] = Seq(), config: Config = defaultConfig): Try[String] = getOutput(intp, prog, inp, config) flatMap (l => Try{l.mkString})
  
  def testInterp(intp: Interpreter, config: Config, prog: String, inp: Seq[Char] = Seq())(f: Try[LazyList[Char]] => Boolean): Boolean = f(intp(config)(prog) map (i => i(inp)))
  def outputEquals(exp: String)(lop: Try[LazyList[Char]]): Boolean = lop.map(lst => lst.mkString == exp).getOrElse(false)
}