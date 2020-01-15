package funge

import common.Interpreter
import common_test.EsoSpec

import scala.io.Source
import scala.util.{Success, Try}
import scala.util.matching.Regex

abstract class FungeSpec extends EsoSpec{
  private val badReg: Regex = raw"""\s*BAD:.*\z""".r
  def getMycology: Option[String] = Try{
    val iFile = Source.fromFile("mycology.b98")
    val prog = iFile.mkString
    iFile.close()
    prog} match{
    case Success(prog) => Some(prog)
    case _ => None}
  def passes(res: Try[LazyList[Char]]): Boolean = res match{
    case Success(lst) => Try{
      lst
        .mkString
        .linesIterator
        .forall(str => !badReg.matches(str))}
      .getOrElse(false)
    case _ => false}
  def testMycology(bfi: Interpreter): Boolean = {
    val progOp = getMycology
    val inp = Seq()
    progOp.exists(prog => testInterp(bfi, defaultConfig, prog, inp)(passes))}
}