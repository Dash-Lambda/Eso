package ui

import brainfuck.{BFOpt, Ook}
import common_test.EsoSpec
import metatape.Metatape

import scala.collection.immutable
import scala.util.Success

abstract class InterfaceHandlerSpec extends EsoSpec{
  val defaultState: EsoRunState = EsoRunState.default
  def defaultIO(strs: String*): EsoTestInterface = EsoTestInterface(strs)
}

class RunProgHandlerSpec extends InterfaceHandlerSpec{
  def runWithArgs(eio: EsoTestInterface = defaultIO())(ops: (String, String)*)(args: (String, String)*): String = {
    val handler = RunProgHandler(eio)
    val state = EsoRunState.withOps(ops)
    handler(state)(mkMap(args.toVector))
    eio.collectOutput()}
  
  "RunProgHandler" should "fail on an unknown file extension" in {
    val res = runWithArgs()()("s" -> "testResources/hworldb.txt")
    assertResult("Error: Unrecognized Language or File Extension\n")(res)}
  
  it should "run hworldb.txt correctly with language override arg" in {
    val res = runWithArgs()()("s" -> "testResources/hworldb.txt", "l" -> "BrainFuck")
    assertResult("Hello World!\n\n")(res)}
  
  it should "recognize known file extensions" in {
    val res = runWithArgs()()("s" -> "testResources/hworld.b")
    assertResult("Hello World!\n\n")(res)}
  
  it should "print ASCII values if printNum is on" in {
    val res = runWithArgs()("printNum" -> "true")("s" -> "testResources/hworld.b")
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
    val res = runWithArgs()("log" -> "true")("s" -> "testResources/hworld.b").replaceAll("""\d+ms""", "NUMms")
    assertResult(chkStr)(res)}
  
  it should "print the program duration if time is on" in {
    val chkStr =
      s"""|Hello World!
          |
          |Program completed in NUMms
          |""".stripMargin.replaceAllLiterally("\r", "")
    val res = runWithArgs()("time" -> "true")("s" -> "testResources/hworld.b").replaceAll("""\d+ms""", "NUMms")
    assertResult(chkStr)(res)}
  
  it should "fail to read input if an inaccessible file is given" in {
    val chkStr = "Error: java.io.FileNotFoundException: testResources\\invalid.txt (The system cannot find the file specified)\n"
    val res = runWithArgs()()("s" -> "testResources/cat.b", "i" -> "testResources/invalid.txt")
    assertResult(chkStr)(res)}
  
  it should "read input from a file if given one" in {
    val chkStr = grabFile("randomNonsense.txt") + '\n'
    val res = runWithArgs()("echoFileInp" -> "false")("s" -> "testResources/cat.b", "i" -> "testResources/randomNonsense.txt")
    assertResult(chkStr)(res)}
  
  it should "correctly append console input to file input if appendInp is on" in {
    val inpStr = "and test"
    val chkStr = s"${grabFile("randomNonsense.txt")} $inpStr\n"
    val res = runWithArgs(defaultIO(inpStr + 0.toChar))("echoFileInp" -> "false", "appendInp" -> "true", "fileEOF" -> "' '")("s" -> "testResources/cat.b", "i" -> "testResources/randomNonsense.txt")
    assertResult(chkStr)(res)}
  
  it should "not append console input to file input if appendInp is off" in {
    val inpStr = "and test"
    val chkStr = grabFile("randomNonsense.txt") + '\n'
    val res = runWithArgs(defaultIO(inpStr))("echoFileInp" -> "false", "appendInp" -> "false")("s" -> "testResources/cat.b", "i" -> "testResources/randomNonsense.txt")
    assertResult(chkStr)(res)}
  
  it should "limit output length if olen >= 0" in {
    val res = runWithArgs()("olen" -> "10")("s" -> "testResources/hworld.b")
    assertResult("Hello Worl\n")(res)}
  
  it should "build a translator path if needed" in {
    val res = runWithArgs()()("s" -> "testResources/hworld.fl")
    assertResult("Hello World!\n\n")(res)}
}

class TranslateHandlerSpec extends InterfaceHandlerSpec{
  def runWithArgs(eio: EsoTestInterface = defaultIO())(ops: (String, String)*)(args: (String, String)*): String = {
    val handler = TranslateHandler(eio)
    val state = EsoRunState.withOps(ops)
    handler(state)(mkMap(args.toVector))
    eio.collectOutput()}
  
  "TranslateHandler" should "fail on unknown file extensions" in {
    val prog = runWithArgs()()("s" -> "testResources/hworldfl.txt", "tl" -> "BrainFuck")
    assertResult("Error: Unrecognized Source Language or File Extension\n")(prog)}
  
  it should "translate hworldfl.txt to BrainFuck correctly with language override" in {
    val prog = runWithArgs()()("s" -> "testResources/hworldfl.txt", "sl" -> "FlufflePuff", "tl" -> "BrainFuck")
    val res = getOutputString(BFOpt, prog)
    assertResult(Success("Hello World!\n"))(res)}
  
  it should "recognize known file extensions" in {
    val prog = runWithArgs()()("s" -> "testResources/hworld.b", "tl" -> "BrainFuck")
    val res = getOutputString(BFOpt, prog)
    assertResult(Success("Hello World!\n"))(res)}
  
  it should "chain translators if needed" in {
    val prog1 = runWithArgs()()("s" -> "testResources/hworld.fl", "tl" -> "Ook")
    val prog2 = Ook(defaultConfig)(prog1)
    val res = prog2 flatMap (p => getOutputString(BFOpt, p))
    assertResult(Success("Hello World!\n"))(res)}
}

class TranspileHandlerSpec extends InterfaceHandlerSpec{
  def runWithArgs(eio: EsoTestInterface = defaultIO())(ops: (String, String)*)(args: (String, String)*): String = {
    val handler = TranspileHandler(eio)
    val state = EsoRunState.withOps(ops)
    handler(state)(mkMap(args.toVector))
    eio.collectOutput()}
  
  "TranspileHandler" should "fail on unknown file extensions" in {
    val prog = runWithArgs()()("s" -> "testResources/hworldb.txt", "tl" -> "Metatape")
    assertResult("Error: Unrecognized Source Language or File Extension\n")(prog)}
  
  it should "transpile hworldb.txt to Metatape correctly with language override" in {
    val prog = runWithArgs()()("s" -> "testResources/hworldb.txt", "tl" -> "Metatape", "sl" -> "BrainFuck")
    val res = getOutputString(Metatape, prog)
    assertResult(Success("Hello World!\n"))(res)}
  
  it should "recognize known file extensions" in {
    val prog = runWithArgs()()("s" -> "testResources/hworld.b", "tl" -> "Metatape")
    val res = getOutputString(Metatape, prog)
    assertResult(Success("Hello World!\n"))(res)}
  
  it should "build a translator path if needed" in {
    val prog = runWithArgs()()("s" -> "testResources/hworld.fl", "tl" -> "Metatape")
    val res = getOutputString(Metatape, prog)
    assertResult(Success("Hello World!\n"))(res)}
}

class DefineBFLangHandlerSpec extends InterfaceHandlerSpec{
  def runWithArgs(eio: EsoTestInterface = defaultIO())(ops: (String, String)*)(args: (String, String)*): EsoState = {
    val handler = DefineBFLangHandler(eio)
    val state = EsoRunState.withOps(ops)
    handler(state)(mkMap(args.toVector))}
  
  "DefineBFLangHandler" should "correctly define a BF lang" in {
    val finState = runWithArgs(defaultIO("test", "1", "2", "3", "4", "5", "6", "7", "8"))()()
    finState match{
      case EsoRunState(_, _, ts, _, _, _) => assert(ts.isDefinedAt(("test", "BrainFuck")))
      case _ => fail("Returned Improper State")}}
}

class SetVarHandlerSpec extends InterfaceHandlerSpec{
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
      val testState = SetVarHandler(falseState)(immutable.HashMap("log" -> s))
      testState match{
        case EsoRunState(_, _, _, bools, _, _) => assert(bools("log"), s"'$s' gave false")
        case _ => fail("Did Not Return RunState")}}}
  
  it should "recognize all false keywords" in {
    for(s <- falses){
      val testState = SetVarHandler(trueState)(immutable.HashMap("log" -> s))
      testState match{
        case EsoRunState(_, _, _, bools, _, _) => assert(!bools("log"), s"'$s' gave true")
        case _ => fail("Did Not Return RunState")}}}
  
  it should "recognize positive and negative numbers" in {
    for(n <- Seq(-100, -10, 0, 10, 100)){
      val testState = SetVarHandler(numInitState)(immutable.HashMap("olen" -> n.toString))
      testState match{
        case EsoRunState(_, _, _, _, nums, _) => assertResult(nums("olen"))(n)
        case _ => fail("Did Not Return RunState")}}}
  
  it should "recognize character input" in {
    for(c <- 'a' to 'z'){
      val testState = SetVarHandler(numInitState)(immutable.HashMap("olen" -> s"'$c'"))
      testState match{
        case EsoRunState(_, _, _, _, nums, _) => assertResult(nums("olen").toChar)(c)
        case _ => fail("Did Not Return RunState")}}}
  
  it should "recognize multiple assignments" in {
    val testState = SetVarHandler(multiInitState)(multiMapping)
    testState match{
      case EsoRunState(_, _, _, bools, nums, _) =>
        for(nam <- boolNames){
          withClue(s"Failure on variable '$nam'"){
            assertResult(true)(bools(nam))}}
        for(nam <- numNames){
          withClue(s"Failure on variable '$nam'"){
            assertResult(1)(nums(nam))}}
      case _ => fail("Did Not Return RunState")}}
}