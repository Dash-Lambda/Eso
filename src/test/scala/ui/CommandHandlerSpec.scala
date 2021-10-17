package ui

import brainfuck.{BFOpt, BFTranslator, GenBFT, Ook}
import common_test.EsoSpec
import metatape.Metatape

import scala.collection.immutable
import scala.util.{Failure, Success}

abstract class CommandHandlerSpec extends EsoSpec{
  val defaultState: EsoRunState = EsoRunState.default
  def defaultIO(strs: String*): EsoTestInterface = EsoTestInterface(strs)
  
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler
  def runWithArgs(eio: EsoTestInterface = defaultIO(), efi: EsoFileInterface = SystemFileInterface)(ops: (String, String)*)(args: (String, String)*): (EsoState, String) = {
    val state = EsoRunState.withOpSeq(ops)
    runner(eio, efi, state, args)}
  def runStateWithArgs(eio: EsoTestInterface = defaultIO(), efi: EsoFileInterface = SystemFileInterface)(state: EsoRunState = EsoRunState.default)(args: (String, String)*): (EsoState, String) = {
    runner(eio, efi, state, args)}
  def runner(eio: EsoTestInterface, efi: EsoFileInterface, state: EsoRunState, args: Seq[(String, String)]): (EsoState, String) = {
    val handler = currentHandler(eio, efi)
    val ns = handler(state)(mkMap(args.toVector))
    (ns, eio.collectOutput())}
}

class RunProgHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = RunProgHandler(eio, efi)
  
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
          |Initializing interpreter... Done.
          |Hello World!
          |
          |""".stripMargin.replace("\r", "")
    val res = runWithArgs()("log" -> "true")("s" -> "testResources/hworld.b")._2
    assertResult(chkStr)(res)}
  
  it should "print the program duration if time is on" in {
    val chkStr =
      s"""|Hello World!
          |
          |Program completed in NUMms
          |""".stripMargin.replace("\r", "")
    val res = runWithArgs()("time" -> "true")("s" -> "testResources/hworld.b")._2.replaceAll("""\d+ms""", "NUMms")
    assertResult(chkStr)(res)}
  
  it should "print initialization time if log and time flags are on" in {
    val chkStr =
      s"""|Searching for translator path... Done.
          |Retrieving program from file... Done.
          |Translating program... Done.
          |Initializing interpreter... Done in NUMms.
          |Hello World!
          |
          |Program completed in NUMms
          |""".stripMargin.replace("\r", "")
    val res = runWithArgs()("log" -> "true", "time" -> "true")("s" -> "testResources/hworld.b")._2.replaceAll("""\d+ms""", "NUMms")
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
  
  it should "cache built programs if the cache flag is on" in {
    val cacheState = runWithArgs()("log" -> "true")("s" -> "testResources/hworld.b")
    cacheState match{
      case (rs: EsoRunState, _) =>
        val eio = defaultIO()
        runStateWithArgs(eio)(rs)("s" -> "testResources/hworld.b")
        val res = eio.collectOutput()
        assertResult("Using cached run (disable with 'set -cache off')\nHello World!\n\n")(res)
      case _ => fail("Unexpected Halt State")}}
  
  it should "not cache built programs if the cache flag is off" in {
    val cacheState = runWithArgs()("log" -> "true", "cache" -> "false")("s" -> "testResources/hworld.b")
    cacheState match{
      case (rs: EsoRunState, _) =>
        val eio = defaultIO()
        runStateWithArgs(eio)(rs)("s" -> "testResources/hworld.b")
        val res = eio.collectOutput()
        val chkStr =
          s"""|Searching for translator path... Done.
              |Retrieving program from file... Done.
              |Translating program... Done.
              |Initializing interpreter... Done.
              |Hello World!
              |
              |""".stripMargin.replace("\r", "")
        assertResult(chkStr)(res)
      case _ => fail("Unexpected Halt State")}}
  
  it should "write program output to a file with the -o option" in {
    val efi = MutableContainedFileInterface.withElms("hworld.b" -> (grabFile("hworld.b"), 0))
    runWithArgs(efi=efi)()("s" -> "hworld.b", "o" -> "testOutput.txt")
    val fo = efi.readFile("testOutput.txt")
    assertResult(Success("Hello World!\n"))(fo)}
  
  it should "not write program output to a file without the -o option" in {
    val efi = MutableContainedFileInterface.withElms("hworld.b" -> (grabFile("hworld.b"), 0))
    runWithArgs(efi=efi)()("s" -> "hworld.b")
    assert(!efi.fileExists("testOutput.txt"))}
  
  it should "fail to read program if an inaccessible file is given" in {
    val res = runWithArgs()()("s" -> "testResources/nope.b")._2
    assert(res.startsWith("Error: java.io.FileNotFoundException"))}
}

class TranslateHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = TranslateHandler(eio, efi)
  
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
  
  it should "write translation to a file with the -o option" in {
    val efi = MutableContainedFileInterface.withElms("hworldfl.txt" -> (grabFile("hworldfl.txt"), 0))
    runWithArgs(efi=efi)()("s" -> "hworldfl.txt", "sl" -> "FlufflePuff", "tl" -> "BrainFuck", "o" -> "testOutput.txt")._2
    assert(efi.fileExists("testOutput.txt"))}
  
  it should "not write translation to a file without the -o option" in {
    val efi = MutableContainedFileInterface.withElms("hworldfl.txt" -> (grabFile("hworldfl.txt"), 0))
    runWithArgs(efi=efi)()("s" -> "hworldfl.txt", "sl" -> "FlufflePuff", "tl" -> "BrainFuck")
    assert(!efi.fileExists("testOutput.txt"))}
}

class TranspileHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = TranspileHandler(eio, efi)
  
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
  
  it should "write transpilation to a file with the -o option" in {
    val efi = MutableContainedFileInterface.withElms("hworldb.txt" -> (grabFile("hworldb.txt"), 0))
    runWithArgs(efi=efi)()("s" -> "hworldb.txt", "tl" -> "Metatape", "sl" -> "BrainFuck", "o" -> "testOutput.txt")._2
    assert(efi.fileExists("testOutput.txt"))}
  
  it should "not write transpilation to a file without the -o option" in {
    val efi = MutableContainedFileInterface.withElms("hworldb.txt" -> (grabFile("hworldb.txt"), 0))
    runWithArgs(efi=efi)()("s" -> "hworldb.txt", "tl" -> "Metatape", "sl" -> "BrainFuck")
    assert(!efi.fileExists("testOutput.txt"))}
}

class DefineBFLangHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = DefineBFLangHandler(eio)
  
  "DefineBFLangHandler" should "correctly define a BF lang" in {
    val finState = runWithArgs(defaultIO("test", "1", "2", "3", "4", "5", "6", "7", "8"))()()._1
    finState match{
      case EsoRunState(_, _, ts, _, _, _, _, _) => assert(ts.isDefinedAt(("test", "BrainFuck")))
      case _ => fail("Returned Improper State")}}
}

class LoadBFLangsHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = LoadBFLangsHandler(eio, efi)
  
  val langFile: String = grabFile("testLangs.json")
  val pairs: Vector[(String, String)] = Vector(
    "[" -> "a",
    "]" -> "b",
    "<" -> "c",
    ">" -> "d",
    "+" -> "e",
    "-" -> "f",
    "," -> "g",
    "." -> "h")
  
  "LoadBFLangsHandlerSpec" should "fail on an inaccessible file" in {
    val efi = MutableContainedFileInterface.withElms()
    val (_, str) = runWithArgs(efi=efi)()("f" -> "fail")
    withClue(s"Error: $str"){
      assert(str.startsWith("Error: common.EsoExcep (File Not Found)"))}}
  
  it should "correctly load languages from a file" in {
    val efi = MutableContainedFileInterface.withElms("testLangFile.json" -> (langFile, 0L))
    val (ns, str) = runWithArgs(efi=efi)()("f" -> "testLangFile.json")
    assertResult("Loaded BF Langs:\n- test\n")(str)
    ns match{
      case EsoRunState(_, _, ts, _, _, _, _, _) => ts.get(("test", "BrainFuck")) match{
        case Some(t: BFTranslator) =>
          val res = t.kvPairs.sortBy(_._2)
          assertResult(pairs)(res)
        case _ => fail("Didn't Load Translator with Correct ID")}
      case _ => fail("Returned Improper State")}}
  
  it should "correctly load languages through loadOnly" in {
    val efi = MutableContainedFileInterface.withElms("BFLangs.json" -> (langFile, 0L))
    val ns = LoadBFLangsHandler(EsoDummyInterface, efi).loadOnly(EsoRunState.default)
    ns match{
      case EsoRunState(_, _, ts, _, _, _, _, _) => ts.get(("test", "BrainFuck")) match{
        case Some(t: BFTranslator) =>
          val res = t.kvPairs.sortBy(_._2)
          assertResult(pairs)(res)
        case _ => fail("Didn't Load Translator with Correct ID")}
      case _ => fail("Returned Improper State")}}
}

class SaveBFLangsHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = SaveBFLangsHandler(eio, efi)
  
  val bft: GenBFT = GenBFT("test", Vector(
    "[" -> "0",
    "]" -> "1",
    "<" -> "2",
    ">" -> "3",
    "+" -> "4",
    "-" -> "5",
    "," -> "6",
    "." -> "7"))
  
  "SaveBFLangsHandler" should "save to the default file if none is provided" in {
    val state = EsoRunState.default.addTrans(bft)
    val efi = MutableContainedFileInterface.withElms()
    runStateWithArgs(efi=efi)(state)()
    assert(efi.fileExists(EsoDefaults.defBFLFile))}
  
  it should "save to the provided file with -f option" in {
    val state = EsoRunState.default.addTrans(bft)
    val efi = MutableContainedFileInterface.withElms()
    runStateWithArgs(efi=efi)(state)("f" -> "testOutput.txt")
    assert(efi.fileExists("testOutput.txt"))}
}

class ShowSyntaxHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = ShowSyntaxHandler(eio)
  
  val ref: String =
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
       |""".stripMargin.replace("\r", "")
  
  "ShowSyntaxHandler" should "correctly display syntax for FlufflePuff" in {
    val res = runWithArgs()()("l" -> "FlufflePuff")._2
    assertResult(ref)(res)}
}

class ClearBindingsHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = ClearBindingsHandler
  
  "ClearBindingsHandler" should "clear all bindings" in {
    val state = EsoRunState.default.addBind("test", "ping")
    val (res, _) = runStateWithArgs()(state)()
    res match{
      case rs: EsoRunState =>
        val chk = rs.binds.get("test")
        assertResult(None)(chk)
      case _ => fail("Returned Invalid State")}}
}

class ClearCacheHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = ClearCacheHandler
  
  "ClearCacheHandler" should "clear the built program cache" in {
    val state = EsoRunState.default.cacheFunc("test", "l", 0, _ => LazyList())
    val (res, _) = runStateWithArgs()(state)()
    res match{
      case rs: EsoRunState =>
        val chk = rs.runCache.get("test" -> "l")
        assertResult(None)(chk)
      case _ => fail("Returned Invalid State")}}
}

class LoadBindingsHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = LoadBindingsHandler(eio, efi)
  
  val bindFile: String = grabFile("testBindings.json")
  
  "LoadBindingsHandler" should "correctly load bindings from provided file using -f option" in {
    val efi = MutableContainedFileInterface.withElms("testBindings.json" -> (bindFile, 0))
    val (state, _) = runWithArgs(efi=efi)()("f" -> "testBindings.json")
    state match{
      case rs: EsoRunState =>
        val chk = rs.binds.get("test")
        assertResult(Some("testCommand"))(chk)}}
  
  it should "correctly load bindings from default file with no arguments" in {
    val efi = MutableContainedFileInterface.withElms("userBindings.json" -> (bindFile, 0))
    val (state, _) = runWithArgs(efi=efi)()()
    state match{
      case rs: EsoRunState =>
        val chk = rs.binds.get("test")
        assertResult(Some("testCommand"))(chk)}}
  
  it should "correctly load bindings from default file with loadOnly" in {
    val efi = MutableContainedFileInterface.withElms("userBindings.json" -> (bindFile, 0))
    val state = LoadBindingsHandler(EsoDummyInterface, efi).loadOnly(EsoRunState.default)
    state match{
      case rs: EsoRunState =>
        val chk = rs.binds.get("test")
        assertResult(Some("testCommand"))(chk)}}
}

class SaveBindingsHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = SaveBindingsHandler(efi)
  
  "SaveBindingsHandler" should "correctly save bindings to default file with no arguments" in {
    val efi = MutableContainedFileInterface.withElms()
    val state = EsoRunState.default.addBind("test", "testCommand")
    runStateWithArgs(efi=efi)(state)()
    val chk = efi.getFile("userBindings.json")
    assertResult(Some("""{"names":["test"],"test":"testCommand"}"""))(chk)}
  
  it should "correctly save bindings to provided file using -f option" in {
    val efi = MutableContainedFileInterface.withElms()
    val state = EsoRunState.default.addBind("test", "testCommand")
    runStateWithArgs(efi=efi)(state)("f" -> "testBindings.json")
    val chk = efi.getFile("testBindings.json")
    assertResult(Some("""{"names":["test"],"test":"testCommand"}"""))(chk)}
}

class ListBindingsHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = ListBindingsHandler(eio)
  
  "ListBindingsHandler" should "correctly list the current bindings" in {
    val binds = (('a' to 'z') zip (1 to 26)).toVector
    val state = binds.foldLeft(EsoRunState.default){case (s, (k, v)) => s.addBind(k.toString, v.toString)}
    val (_, res) = runStateWithArgs()(state)()
    val ref =
      s"""|Bindings...
          |${binds.map{case (k, v) => s"- $k => $v"}.sorted.mkString("\n")}
          |
          |""".stripMargin
    assertResult(normLines(ref))(normLines(res))}
}

class SetVarHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = SetVarHandler(eio)
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
  
  val trues: LazyList[String] = LazyList.range(1, 10).map(_.toString) #::: LazyList("t", "true", "y", "yes", "on", "i").flatMap(permuteCases)
  val falses: LazyList[String] = LazyList("0", "f", "false", "n", "no", "off", "o").flatMap(permuteCases)
  
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
        case EsoRunState(_, _, _, bools, _, _, _, _) => assert(bools("log"), s"'$s' gave false")
        case _ => fail("Did Not Return RunState")}}}
  
  it should "recognize all false keywords" in {
    for(s <- falses){
      val testState = SetVarHandler()(trueState)(immutable.HashMap("log" -> s))
      testState match{
        case EsoRunState(_, _, _, bools, _, _, _, _) => assert(!bools("log"), s"'$s' gave true")
        case _ => fail("Did Not Return RunState")}}}
  
  it should "recognize positive and negative numbers" in {
    for(n <- Seq(-100, -10, 0, 10, 100)){
      val testState = SetVarHandler()(numInitState)(immutable.HashMap("olen" -> n.toString))
      testState match{
        case EsoRunState(_, _, _, _, nums, _, _, _) => assertResult(nums("olen"))(n)
        case _ => fail("Did Not Return RunState")}}}
  
  it should "recognize character input" in {
    for(c <- 'a' to 'z'){
      val testState = SetVarHandler()(numInitState)(immutable.HashMap("olen" -> s"'$c'"))
      testState match{
        case EsoRunState(_, _, _, _, nums, _, _, _) => assertResult(nums("olen").toChar)(c)
        case _ => fail("Did Not Return RunState")}}}
  
  it should "recognize multiple assignments" in {
    val testState = SetVarHandler()(multiInitState)(multiMapping)
    testState match{
      case EsoRunState(_, _, _, bools, nums, _, _, _) =>
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

class SetDefaultsHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = SetDefaultsHandler
  
  "SetDefaultHandler" should "reset the state to defaults" in {
    val state = EsoRunState(immutable.HashMap(), immutable.HashMap(), immutable.HashMap(), immutable.HashMap(), immutable.HashMap(), immutable.HashMap(), immutable.HashMap(), immutable.HashMap())
    val (res, _) = runStateWithArgs()(state)()
    assertResult(EsoRunState.default)(res)}
}

class ListLangsHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = ListLangsHandler(eio)
  
  val ref: String = grabFile("list_langs_reference.txt")
  
  "ListLangsHandler" should "correctly list the default language components" in {
    val (_, res) = runWithArgs()()()
    assertResult(normLines(ref))(normLines(res))}
}

class listVarsHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = ListVarsHandler(eio)
  
  val ref: String = grabFile("help_reference.txt")
  
  "listVarsHandler" should "correctly list the default runtime parameters" in {
    val (_, res) = runWithArgs()()()
    assertResult(normLines(ref))(normLines(res))}
}

class listFileAssociationsHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = ListFileAssociationsHandler(eio)
  
  val ref: String = grabFile("file_association_reference.txt")
  
  "ListFileAssociationsHandler" should "correctly list the default file associations" in {
    val (_, res) = runWithArgs()()()
    assertResult(normLines(ref))(normLines(res))}
  
  it should "correctly list the current file associations" in {
    val ref2 =
      """|File Associations...
         |- .e1 => l1
         |- .e2 => l2
         |
         |""".stripMargin
    val assoc = immutable.HashMap("e1" -> "l1", "e2" -> "l2")
    val state = EsoRunState.withItems(fileAssoc = assoc)
    val (_, res) = runStateWithArgs()(state)()
    assertResult(normLines(ref2))(normLines(res))}
}

class SaveFileAssociationsHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = SaveFileAssociationsHandler(efi)
  
  val refStr: String = grabFile("file_association_reference.json")
  
  "SaveFileAssociationsHandler" should "correctly save file associations to the default file with no arguments" in {
    val efi = MutableContainedFileInterface.withElms()
    runWithArgs(efi=efi)()()
    efi.readFile("fileAssoc.json") match{
      case Success(res) => assertResult(refStr)(res)
      case Failure(e) => fail(s"File Read Error $e")}}
  
  it should "correctly save file associations to the provided file with -f option" in {
    val efi = MutableContainedFileInterface.withElms()
    runWithArgs(efi=efi)()("f" -> "testAssoc.json")
    efi.readFile("testAssoc.json") match{
      case Success(res) => assertResult(refStr)(res)
      case Failure(e) => fail(s"File Read Error $e")}}
}

class LoadFileAssociationsHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = LoadFileAssociationsHandler(eio, efi)
  
  "LoadFileAssociationsHandler" should "correctly load file associations from the default file with no arguments" in {
    val efi = MutableContainedFileInterface.withElms(
      "fileAssoc.json" -> ("""{"e1":"l1","e2":"l2","names":["e1","e2"]}""", 0))
    val state = EsoRunState.withItems()
    runStateWithArgs(efi=efi)(state)() match{
      case (rs: EsoRunState, _) =>
        assertResult(Some("l1"))(rs.fileAssoc.get("e1"))
        assertResult(Some("l2"))(rs.fileAssoc.get("e2"))
      case _ => fail("Unexpected Halt State")}}
  
  it should "correctly load file associations from the provided file with -f" in {
    val efi = MutableContainedFileInterface.withElms(
      "testAssoc.json" -> ("""{"e1":"l1","e2":"l2","names":["e1","e2"]}""", 0))
    val state = EsoRunState.withItems()
    runStateWithArgs(efi=efi)(state)("f" -> "testAssoc.json") match{
      case (rs: EsoRunState, _) =>
        assertResult(Some("l1"))(rs.fileAssoc.get("e1"))
        assertResult(Some("l2"))(rs.fileAssoc.get("e2"))
      case _ => fail("Unexpected Halt State")}}
  
  it should "correctly load file associations from the default file with loadOnly" in {
    val efi = MutableContainedFileInterface.withElms(
      "fileAssoc.json" -> ("""{"e1":"l1","e2":"l2","names":["e1","e2"]}""", 0))
    val state = EsoRunState.withItems()
    LoadFileAssociationsHandler(EsoDummyInterface, efi).loadOnly(state) match{
      case rs: EsoRunState =>
        assertResult(Some("l1"))(rs.fileAssoc.get("e1"))
        assertResult(Some("l2"))(rs.fileAssoc.get("e2"))
      case _ => fail("Unexpected Halt State")}}
}

class AddFileAssociationHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = AddFileAssociationHandler
  
  "AddFileAssociationHandler" should "add individual file associations" in {
    val assoc = immutable.HashMap("e1" -> "l1")
    val state = EsoRunState.withItems(fileAssoc = assoc)
    runStateWithArgs()(state)("e2" -> "l2") match{
      case (rs: EsoRunState, _) =>
        assert(rs.fileAssoc.isDefinedAt("e1"))
        assertResult(Some("l2"))(rs.fileAssoc.get("e2"))
      case _ => fail("Unexpected Halt State")}}
  
  it should "add multiple file associations" in {
    val assoc = immutable.HashMap("e1" -> "l1")
    val state = EsoRunState.withItems(fileAssoc = assoc)
    runStateWithArgs()(state)("e2" -> "l2", "e3" -> "l3") match{
      case (rs: EsoRunState, _) =>
        assert(rs.fileAssoc.isDefinedAt("e1"))
        assertResult(Some("l2"))(rs.fileAssoc.get("e2"))
        assertResult(Some("l3"))(rs.fileAssoc.get("e3"))
      case _ => fail("Unexpected Halt State")}}
}

class DropFileAssociationHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = DropFileAssociationHandler
  
  "DropFileAssociationHandler" should "drop individual file associations" in {
    val assoc = immutable.HashMap("e1" -> "l1", "e2" -> "l2")
    val state = EsoRunState.withItems(fileAssoc = assoc)
    runStateWithArgs()(state)("e" -> "e1") match{
      case (rs: EsoRunState, _) =>
        assert(!rs.fileAssoc.isDefinedAt("e1"))
        assert(rs.fileAssoc.isDefinedAt("e2"))
      case _ => fail("Unexpected Halt State")}}
}

class ClearFileAssociationsHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = ClearFileAssociationsHandler
  
  "ClearFileAssociationsHandler" should "clear all file associations" in {
    val assoc = immutable.HashMap("e1" -> "l1", "e2" -> "l2")
    val state = EsoRunState.withItems(fileAssoc = assoc)
    runStateWithArgs()(state)() match{
      case (rs: EsoRunState, _) => assert(rs.fileAssoc.isEmpty)
      case _ => fail("Unexpected Halt State")}}
}

class ExitHandlerSpec extends CommandHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): CommandHandler = ExitHandler
  
  "ExitHandler" should "return the halt state" in {
    val (res, _) = runWithArgs()()()
    assertResult(EsoHalt)(res)}
}