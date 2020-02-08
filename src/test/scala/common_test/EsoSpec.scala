package common_test

import common.{Config, Interpreter}
import org.scalatest.flatspec.AnyFlatSpec
import ui.{EsoFileReader, EsoRunState}

import scala.collection.immutable
import scala.util.{Success, Try}

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
  
  def testAgainstOutput(intp: Interpreter, config: Config = defaultConfig, first: Boolean = false)(nam: String, prog: String, inp: String, ref: String): Unit = {
    def runTest(): Unit = {
      val res = config.num("olen") match{
        case -1 => getOutputString(intp, prog, inp, config)
        case n => getOutput(intp, prog, inp, config) map (_.take(n).mkString)}
      assertResult(Success(ref))(res)}
    if(first) intp.name should s"run $nam correctly" in runTest()
    else it should s"run $nam correctly" in runTest}
  def testAllAgainstOutput(intp: Interpreter, config: Config = defaultConfig)(itms: (String, String, String)*): Unit = itms match{
    case (nam1, inp1, ref1) +: rem =>
      testAgainstOutput(intp, config, first=true)(nam1, grabFile(nam1), inp1, ref1)
      for((nam, inp, ref) <- rem){
        testAgainstOutput(intp, config)(nam, grabFile(nam), inp, ref)}}
  def testAllAgainstOutputWithLimit(intp: Interpreter, config: Config = defaultConfig)(itms: (String, String, String, Int)*): Unit = itms match{
    case (nam1, inp1, ref1, lim1) +: rem =>
      testAgainstOutput(intp, config.set("olen", lim1), first=true)(nam1, grabFile(nam1), inp1, ref1)
      for((nam, inp, ref, lim) <- rem){
        testAgainstOutput(intp, config.set("olen", lim))(nam, grabFile(nam), inp, ref)}}
}