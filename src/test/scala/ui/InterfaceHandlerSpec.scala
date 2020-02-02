package ui

import brainfuck.{BFOpt, BFTranslator, Ook}
import common_test.EsoSpec
import metatape.Metatape

import scala.collection.immutable
import scala.util.Success

abstract class InterfaceHandlerSpec extends EsoSpec{
  val defaultState: EsoRunState = EsoRunState.default
  def defaultIO(strs: String*): EsoTestInterface = EsoTestInterface(strs)
  
  def currentHandler: EsoTestInterface => InterfaceHandler
  def runWithArgs(eio: EsoTestInterface = defaultIO())(ops: (String, String)*)(args: (String, String)*): (EsoState, String) = {
    val handler = currentHandler(eio)
    val state = EsoRunState.withOps(ops)
    val ns = handler(state)(mkMap(args.toVector))
    (ns, eio.collectOutput())}
}

class RunProgHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler: EsoTestInterface => InterfaceHandler = RunProgHandler
  
  "RunProgHandler" should "fail on an unknown file extension" in {
    val res = runWithArgs()()("s" -> "testResources/hworldb.txt")._2
    assertResult("Error: Unrecognized Language or File Extension\n")(res)}
  
  it should "run hworldb.txt correctly with language override arg" in {
    val res = runWithArgs()()("s" -> "testResources/hworldb.txt", "l" -> "BrainFuck")._2
    assertResult("Hello World!\n\n")(res)}
  
  it should "recognize known file extensions" in {
    val res = runWithArgs()()("s" -> "testResources/hworld.b")._2
    assertResult("Hello World!\n\n")(res)}
  
  it should "print ASCII values if printNum is on" in {
    val res = runWithArgs()("printNum" -> "true")("s" -> "testResources/hworld.b")._2
    assertResult("72 101 108 108 111 32 87 111 114 108 100 33 10 \n")(res)}
  
  it should "print detailed information if log is on" in {
    val chkStr =
      s"""|Searching for translator path... Done.
          |Retrieving program from file... Done.
          |Translating program... Done.
          |Initializing interpreter... Done in NUMms.
          |Hello World!
          |
          |""".stripMargin.replaceAllLiterally("\r", "")
    val res = runWithArgs()("log" -> "true")("s" -> "testResources/hworld.b")._2.replaceAll("""\d+ms""", "NUMms")
    assertResult(chkStr)(res)}
  
  it should "print the program duration if time is on" in {
    val chkStr =
      s"""|Hello World!
          |
          |Program completed in NUMms
          |""".stripMargin.replaceAllLiterally("\r", "")
    val res = runWithArgs()("time" -> "true")("s" -> "testResources/hworld.b")._2.replaceAll("""\d+ms""", "NUMms")
    assertResult(chkStr)(res)}
  
  it should "fail to read input if an inaccessible file is given" in {
    val res = runWithArgs()()("s" -> "testResources/cat.b", "i" -> "testResources/invalid.txt")._2
    assert(res.startsWith("Error: java.io.FileNotFoundException"))}
  
  it should "read input from a file if given one" in {
    val chkStr = grabFile("randomNonsense.txt") + '\n'
    val res = runWithArgs()("echoFileInp" -> "false")("s" -> "testResources/cat.b", "i" -> "testResources/randomNonsense.txt")._2
    assertResult(chkStr)(res)}
  
  it should "correctly append console input to file input if appendInp is on" in {
    val inpStr = "and test"
    val chkStr = s"${grabFile("randomNonsense.txt")} $inpStr\n"
    val res = runWithArgs(defaultIO(inpStr + 0.toChar))("echoFileInp" -> "false", "appendInp" -> "true", "fileEOF" -> "' '")("s" -> "testResources/cat.b", "i" -> "testResources/randomNonsense.txt")._2
    assertResult(chkStr)(res)}
  
  it should "not append console input to file input if appendInp is off" in {
    val inpStr = "and test"
    val chkStr = grabFile("randomNonsense.txt") + '\n'
    val res = runWithArgs(defaultIO(inpStr))("echoFileInp" -> "false", "appendInp" -> "false")("s" -> "testResources/cat.b", "i" -> "testResources/randomNonsense.txt")._2
    assertResult(chkStr)(res)}
  
  it should "limit output length if olen >= 0" in {
    val res = runWithArgs()("olen" -> "10")("s" -> "testResources/hworld.b")._2
    assertResult("Hello Worl\n")(res)}
  
  it should "build a translator path if needed" in {
    val res = runWithArgs()()("s" -> "testResources/hworld.fl")._2
    assertResult("Hello World!\n\n")(res)}
}

class TranslateHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler: EsoTestInterface => InterfaceHandler = TranslateHandler
  
  "TranslateHandler" should "fail on unknown file extensions" in {
    val prog = runWithArgs()()("s" -> "testResources/hworldfl.txt", "tl" -> "BrainFuck")._2
    assertResult("Error: Unrecognized Source Language or File Extension\n")(prog)}
  
  it should "translate hworldfl.txt to BrainFuck correctly with language override" in {
    val prog = runWithArgs()()("s" -> "testResources/hworldfl.txt", "sl" -> "FlufflePuff", "tl" -> "BrainFuck")._2
    val res = getOutputString(BFOpt, prog)
    assertResult(Success("Hello World!\n"))(res)}
  
  it should "recognize known file extensions" in {
    val prog = runWithArgs()()("s" -> "testResources/hworld.b", "tl" -> "BrainFuck")._2
    val res = getOutputString(BFOpt, prog)
    assertResult(Success("Hello World!\n"))(res)}
  
  it should "chain translators if needed" in {
    val prog1 = runWithArgs()()("s" -> "testResources/hworld.fl", "tl" -> "Ook")._2
    val prog2 = Ook(defaultConfig)(prog1)
    val res = prog2 flatMap (p => getOutputString(BFOpt, p))
    assertResult(Success("Hello World!\n"))(res)}
}

class TranspileHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler: EsoTestInterface => InterfaceHandler = TranspileHandler
  
  "TranspileHandler" should "fail on unknown file extensions" in {
    val prog = runWithArgs()()("s" -> "testResources/hworldb.txt", "tl" -> "Metatape")._2
    assertResult("Error: Unrecognized Source Language or File Extension\n")(prog)}
  
  it should "transpile hworldb.txt to Metatape correctly with language override" in {
    val prog = runWithArgs()()("s" -> "testResources/hworldb.txt", "tl" -> "Metatape", "sl" -> "BrainFuck")._2
    val res = getOutputString(Metatape, prog)
    assertResult(Success("Hello World!\n"))(res)}
  
  it should "recognize known file extensions" in {
    val prog = runWithArgs()()("s" -> "testResources/hworld.b", "tl" -> "Metatape")._2
    val res = getOutputString(Metatape, prog)
    assertResult(Success("Hello World!\n"))(res)}
  
  it should "build a translator path if needed" in {
    val prog = runWithArgs()()("s" -> "testResources/hworld.fl", "tl" -> "Metatape")._2
    val res = getOutputString(Metatape, prog)
    assertResult(Success("Hello World!\n"))(res)}
}

class DefineBFLangHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler: EsoTestInterface => InterfaceHandler = DefineBFLangHandler
  
  "DefineBFLangHandler" should "correctly define a BF lang" in {
    val finState = runWithArgs(defaultIO("test", "1", "2", "3", "4", "5", "6", "7", "8"))()()._1
    finState match{
      case EsoRunState(_, _, ts, _, _, _) => assert(ts.isDefinedAt(("test", "BrainFuck")))
      case _ => fail("Returned Improper State")}}
}

class LoadBFLangsHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler: EsoTestInterface => InterfaceHandler = LoadBFLangsHandler
  
  "LoadBFLangsHandlerSpec" should "fail on an inaccessible file" in {
    val (_, str) = runWithArgs()()("f" -> "fail")
    assert(str.startsWith("Error: java.io.FileNotFoundException"))}
  
  it should "correctly load languages from a file" in {
    val pairs = Vector(
      "[" -> "a",
      "]" -> "b",
      "<" -> "c",
      ">" -> "d",
      "+" -> "e",
      "-" -> "f",
      "," -> "g",
      "." -> "h")
    val (ns, str) = runWithArgs()()("f" -> "testResources/testLangs.json")
    assertResult("Loaded BF Langs:\n- test\n")(str)
    ns match{
      case EsoRunState(_, _, ts, _, _, _) => ts.get(("test", "BrainFuck")) match{
        case Some(t: BFTranslator) =>
          val res = t.kvPairs.sortBy(_._2)
          assertResult(pairs)(res)
        case _ => fail("Didn't Load Translator with Correct ID")}
      case _ => fail("Returned Improper State")}}
}

class ShowSyntaxHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler: EsoTestInterface => InterfaceHandler = ShowSyntaxHandler
  
  "ShowSyntaxHandler" should "correctly display syntax for FlufflePuff" in {
    val ref =
      s"""Syntax for FlufflePuff...
         |[: *gasp*
         |]: *pomf*
         |+: pf
         |-: bl
         |>: b
         |<: t
         |.: !
         |,: ?
         |
         |""".stripMargin.replaceAllLiterally("\r", "")
    val res = runWithArgs()()("l" -> "FlufflePuff")._2
    assertResult(ref)(res)}
}

class SetVarHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler: EsoTestInterface => InterfaceHandler = SetVarHandler
  def permuteBin(len: Int): LazyList[Vector[Boolean]] = {
    LazyList
      .from(0)
      .map{n =>
        n.toBinaryString
          .toVector
          .map(_ == '1')}
      .takeWhile(v => v.sizeIs <= len)
      .map{v =>
        v.reverse
          .padTo(len, false)
          .reverse}}
  def applyCases(str: String)(vec: Vector[Boolean]): String = (str.toVector zip vec).map{case (c, b) => if(b) c.toUpper else c.toLower}.mkString
  def permuteCases(str: String): LazyList[String] = permuteBin(str.length).map(applyCases(str))
  
  val trues: LazyList[String] = LazyList.range(1, 10).map(_.toString) #::: LazyList("t", "true", "y", "yes").flatMap(permuteCases)
  val falses: LazyList[String] = "0" #:: LazyList("f", "false", "n", "no").flatMap(permuteCases)
  
  val boolNames: Vector[String] = EsoDefaults.defBoolVec.map(_._1)
  val numNames: Vector[String] = EsoDefaults.defNumVec.map(_._1)
  
  val trueState: EsoRunState = defaultState.setVarSilent("log", "true")
  val falseState: EsoRunState = defaultState.setVarSilent("log", "false")
  val numInitState: EsoRunState = defaultState.setVarSilent("olen", "-1")
  val multiInitState: EsoRunState = (boolNames.map((_, "false")) ++ numNames.map((_, "0"))).foldLeft(defaultState){case (s, (k, v)) => s.setVarSilent(k, v)}
  val multiMapping: immutable.HashMap[String, String] = mkMap(boolNames.map((_, "true")) ++ numNames.map((_, "1")))
  
  "SetVarHandler" should "recognize all true keywords" in {
    for(s <- trues){
      val testState = SetVarHandler()(falseState)(immutable.HashMap("log" -> s))
      testState match{
        case EsoRunState(_, _, _, bools, _, _) => assert(bools("log"), s"'$s' gave false")
        case _ => fail("Did Not Return RunState")}}}
  
  it should "recognize all false keywords" in {
    for(s <- falses){
      val testState = SetVarHandler()(trueState)(immutable.HashMap("log" -> s))
      testState match{
        case EsoRunState(_, _, _, bools, _, _) => assert(!bools("log"), s"'$s' gave true")
        case _ => fail("Did Not Return RunState")}}}
  
  it should "recognize positive and negative numbers" in {
    for(n <- Seq(-100, -10, 0, 10, 100)){
      val testState = SetVarHandler()(numInitState)(immutable.HashMap("olen" -> n.toString))
      testState match{
        case EsoRunState(_, _, _, _, nums, _) => assertResult(nums("olen"))(n)
        case _ => fail("Did Not Return RunState")}}}
  
  it should "recognize character input" in {
    for(c <- 'a' to 'z'){
      val testState = SetVarHandler()(numInitState)(immutable.HashMap("olen" -> s"'$c'"))
      testState match{
        case EsoRunState(_, _, _, _, nums, _) => assertResult(nums("olen").toChar)(c)
        case _ => fail("Did Not Return RunState")}}}
  
  it should "recognize multiple assignments" in {
    val testState = SetVarHandler()(multiInitState)(multiMapping)
    testState match{
      case EsoRunState(_, _, _, bools, nums, _) =>
        for(nam <- boolNames){
          withClue(s"Failure on variable '$nam'"){
            assertResult(true)(bools(nam))}}
        for(nam <- numNames){
          withClue(s"Failure on variable '$nam'"){
            assertResult(1)(nums(nam))}}
      case _ => fail("Did Not Return RunState")}}
  
  it should "report invalid variable names" in {
    val res = runWithArgs()()("fail" -> "0")._2
    assertResult(s"""Error: Invalid Parameter Name or Value for "fail"${'\n'}""")(res)}
  
  it should "report invalid value types" in {
    val res1 = runWithArgs()()("bfOpt" -> "true")._2
    assertResult(s"""Error: Invalid Parameter Name or Value for "bfOpt"${'\n'}""")(res1)
    
    val res2 = runWithArgs()()("log" -> "100")._2
    assertResult(s"""Error: Invalid Parameter Name or Value for "log"${'\n'}""")(res2)}
}