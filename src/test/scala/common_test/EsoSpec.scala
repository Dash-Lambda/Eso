package common_test

import common.{Config, Interpreter, Translator, Transpiler}
import org.scalatest.flatspec.AnyFlatSpec
import ui.{EsoFileReader, EsoRunState}

import scala.collection.immutable
import scala.util.{Failure, Success, Try}

abstract class EsoSpec extends AnyFlatSpec{
  val defaultConfig: Config = EsoRunState.default.config
  
  def grabFile(str: String, normLineBreaks: Boolean = true): String = {
    val tried = EsoFileReader.readFile(s"testResources/$str")
    assume(tried.isSuccess)
    tried.get}
  
  def filterChars(str: String, cs: Seq[Char]): String = str.filter(cs.contains(_))
  def mkMap[A, B](vec: Vector[(A, B)]): immutable.HashMap[A, B] = {
    val builder = immutable.HashMap.newBuilder[A, B]
    builder ++= vec
    builder.result}
  
  def testTranspilerAgainstProgramResult(interp: Interpreter, trans: Transpiler, prog: String, expected: String, inp: Seq[Char] = Seq(), config: Config = defaultConfig): Unit = {
    trans(config)(prog) match{
      case Success(res) => assertOutput(interp, res, expected, inp, config)
      case Failure(e) => fail(s"Transpilation Failed ($e)")}}
  def testAllTranspiledAgainstProgramResult(interp: Interpreter, trans: Transpiler, config: Config = defaultConfig)(itms: (String, String, String)*): Unit = {
    itms match{
      case (nam1, inp1, ref1) +: rem =>
        trans.toString should s"preserve the behavior of $nam1" in testTranspilerAgainstProgramResult(interp, trans, grabFile(nam1), ref1, inp1.toSeq, config)
        for((nam, inp, ref) <- rem) it should s"preserve the behavior of $nam" in testTranspilerAgainstProgramResult(interp, trans, grabFile(nam), ref, inp.toSeq, config)}}
  
  def testTranslatorAgainstOutput(trans: Translator, rev: Boolean, prog: String, expected: String, config: Config = defaultConfig): Unit = {
    val restry = {
      if(rev) trans.unapply(config)(prog)
      else trans(config)(prog)}
    restry match{
      case Success(res) => assertResult(expected)(res)
      case Failure(e) => fail(s"Translation Failed ($e)")}}
  def testAllTranslatedAgainstOutput(trans: Translator, config: Config = defaultConfig)(itms: (String, String, Boolean)*): Unit = {
    itms match{
      case (nam1, ref1, rev1) +: rem =>
        trans.toString should s"preserve the behavior of $nam1 when translating from ${if(rev1) s"${trans.name} to ${trans.baseLang}" else s"${trans.baseLang} to ${trans.name}"}" in{
          testTranslatorAgainstOutput(trans, rev1, grabFile(nam1), ref1, config)}
        for((nam, ref, rev) <- rem){
          it should s"preserve the behavior of $nam when translating from ${if(rev) s"${trans.name} to ${trans.baseLang}" else s"${trans.baseLang} to ${trans.name}"}" in{
            testTranslatorAgainstOutput(trans, rev, grabFile(nam), ref, config)}}}}
  def testTranslatorAgainstProgramResult(intp: Interpreter, trans: Translator, rev: Boolean, prog: String, expected: String, inp: Seq[Char] = Seq(), canBeSame: Boolean = false, config: Config = defaultConfig): Unit = {
    val restry = {
      if(rev) trans.unapply(config)(prog)
      else trans(config)(prog)}
    restry match{
      case Success(res) if canBeSame || res != prog => assertOutput(intp, res, expected, inp, config)
      case Success(_) => fail("Output Identical")
      case Failure(e) => fail(s"Translation Failed ($e)")}}
  def testAllTranslatedAgainstProgramResult(intp: Interpreter, trans: Translator, canBeSame: Boolean = false, config: Config = defaultConfig)(itms: (String, String, String, Boolean)*): Unit = {
    itms match{
      case (nam1, inp1, ref1, rev1) +: rem =>
        trans.toString should s"preserve the behavior of $nam1 when translating from ${if(rev1) s"${trans.name} to ${trans.baseLang}" else s"${trans.baseLang} to ${trans.name}"}" in testTranslatorAgainstProgramResult(intp, trans, rev1, grabFile(nam1), ref1, inp1.toSeq, canBeSame, config)
        for((nam, inp, ref, rev) <- rem){
          it should s"preserve the behavior of $nam when translating from ${if(rev) s"${trans.name} to ${trans.baseLang}" else s"${trans.baseLang} to ${trans.name}"}" in testTranslatorAgainstProgramResult(intp, trans, rev, grabFile(nam), ref, inp, canBeSame, config)}}}
  
  def getOutput(intp: Interpreter, prog: String, inp: Seq[Char] = Seq(), config: Config = defaultConfig): Try[LazyList[Char]] = intp(config)(prog) map (i => i(inp))
  def getOutputString(intp: Interpreter, prog: String, inp: Seq[Char] = Seq(), config: Config = defaultConfig): Try[String] = getOutput(intp, prog, inp, config) flatMap (l => Try{l.mkString})
  
  def testInterp(intp: Interpreter, config: Config, prog: String, inp: Seq[Char] = Seq())(f: Try[LazyList[Char]] => Boolean): Boolean = f(intp(config)(prog) map (i => i(inp)))
  def outputEquals(exp: String)(lop: Try[LazyList[Char]]): Boolean = lop.map(lst => lst.mkString == exp).getOrElse(false)
  
  def assertOutput(intp: Interpreter, prog: String, expected: String, inp: Seq[Char] = Seq(), config: Config = defaultConfig): Unit = {
    val res = getOutputString(intp, prog, inp, config)
    res match{
      case Success(str) => assertResult(expected)(str)
      case Failure(e) => fail(e)}}
  def assertOutputAutoLimit(intp: Interpreter, prog: String, expected: String, inp: Seq[Char] = Seq(), config: Config = defaultConfig): Unit = {
    val res = getOutput(intp, prog) map (_.take(expected.length).mkString)
    res match{
      case Success(str) => assertResult(expected)(str)
      case Failure(e) => fail(e)}}
  
  def testAgainstOutput(intp: Interpreter, config: Config = defaultConfig, first: Boolean = false)(nam: String, prog: String, inp: String, ref: String): Unit = {
    def runTest(): Unit = {
      val res = config.num("olen") match{
        case -1 => getOutputString(intp, prog, inp, config)
        case n => getOutput(intp, prog, inp, config) map (_.take(n).mkString)}
      res match{
        case Success(str) => assertResult(ref)(str)
        case Failure(e) => fail(e)}}
    if(first) intp.name should s"run $nam correctly" in runTest()
    else it should s"run $nam correctly" in runTest}
  def testAllAgainstOutput(intp: Interpreter, config: Config = defaultConfig)(itms: (String, String, String)*): Unit = itms match{
    case (nam1, inp1, ref1) +: rem =>
      testAgainstOutput(intp, config, first=true)(nam1, grabFile(nam1), inp1, ref1)
      for((nam, inp, ref) <- rem){
        testAgainstOutput(intp, config)(nam, grabFile(nam), inp, ref)}}
  def testAllAgainstOutputLimited(intp: Interpreter, config: Config = defaultConfig)(itms: Seq[(String, String, String, Int)]): Unit = itms match{
    case (nam1, inp1, ref1, lim1) +: rem =>
      testAgainstOutput(intp, config.set("olen", lim1), first=true)(nam1, grabFile(nam1), inp1, ref1)
      for((nam, inp, ref, lim) <- rem){
        testAgainstOutput(intp, config.set("olen", lim))(nam, grabFile(nam), inp, ref)}}
  def testAllAgainstOutputWithLimit(intp: Interpreter, config: Config = defaultConfig)(itms: (String, String, String, Int)*): Unit = {
    testAllAgainstOutputLimited(intp, config)(itms)}
  def testAllAgainstOutputAutoLimit(intp: Interpreter, config: Config = defaultConfig)(itms: (String, String, String, Boolean)*): Unit = {
    testAllAgainstOutputLimited(intp, config)(itms.map{case (nam, inp, ref, lim) => (nam, inp, ref, if(lim) ref.length else -1)})}
}