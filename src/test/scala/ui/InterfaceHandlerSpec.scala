package ui

import brainfuck.{BFOpt, BFTranslator, GenBFT, Ook}
import common_test.EsoSpec
import metatape.Metatape

import scala.collection.immutable
import scala.util.{Failure, Success}

abstract class InterfaceHandlerSpec extends EsoSpec{
  val defaultState: EsoRunState = EsoRunState.default
  def defaultIO(strs: String*): EsoTestInterface = EsoTestInterface(strs)
  
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler
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

class RunProgHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = RunProgHandler(eio, efi)
  
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
    val cacheState = runWithArgs()("log" -> "true")("s" -> "hworld.b")
    cacheState match{
      case (rs: EsoRunState, _) =>
        val eio = defaultIO()
        runStateWithArgs(eio)(rs)("s" -> "hworld.b")
        val res = eio.collectOutput()
        assertResult("Using cached run (disable with 'set -cache off')\nHello World!\n\n")(res)
      case _ => fail("Unexpected Halt State")}}
  
  it should "not cache built programs if the cache flag is off" in {
    val cacheState = runWithArgs()("log" -> "true", "cache" -> "false")("s" -> "hworld.b")
    cacheState match{
      case (rs: EsoRunState, _) =>
        val eio = defaultIO()
        runStateWithArgs(eio)(rs)("s" -> "hworld.b")
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
}

class TranslateHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = TranslateHandler(eio, efi)
  
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

class TranspileHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = TranspileHandler(eio, efi)
  
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

class DefineBFLangHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = DefineBFLangHandler(eio)
  
  "DefineBFLangHandler" should "correctly define a BF lang" in {
    val finState = runWithArgs(defaultIO("test", "1", "2", "3", "4", "5", "6", "7", "8"))()()._1
    finState match{
      case EsoRunState(_, _, ts, _, _, _, _, _) => assert(ts.isDefinedAt(("test", "BrainFuck")))
      case _ => fail("Returned Improper State")}}
}

class LoadBFLangsHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = LoadBFLangsHandler(eio, efi)
  
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
    val (_, str) = runWithArgs()()("f" -> "fail")
    assert(str.startsWith("Error: java.io.FileNotFoundException"))}
  
  it should "correctly load languages from a file" in {
    val (ns, str) = runWithArgs()()("f" -> "testResources/testLangs.json")
    assertResult("Loaded BF Langs:\n- test\n")(str)
    ns match{
      case EsoRunState(_, _, ts, _, _, _, _, _) => ts.get(("test", "BrainFuck")) match{
        case Some(t: BFTranslator) =>
          val res = t.kvPairs.sortBy(_._2)
          assertResult(pairs)(res)
        case _ => fail("Didn't Load Translator with Correct ID")}
      case _ => fail("Returned Improper State")}}
}

class SaveBFLangsHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = SaveBFLangsHandler(eio, efi)
  
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

class ShowSyntaxHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = ShowSyntaxHandler(eio)
  
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

class ClearBindingsHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = ClearBindingsHandler
  
  "ClearBindingsHandler" should "clear all bindings" in {
    val state = EsoRunState.default.addBind("test", "ping")
    val (res, _) = runStateWithArgs()(state)()
    res match{
      case rs: EsoRunState =>
        val chk = rs.binds.get("test")
        assertResult(None)(chk)
      case _ => fail("Returned Invalid State")}}
}

class ClearCacheHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = ClearCacheHandler
  
  "ClearCacheHandler" should "clear the built program cache" in {
    val state = EsoRunState.default.cacheFunc("test", "l", 0, _ => LazyList())
    val (res, _) = runStateWithArgs()(state)()
    res match{
      case rs: EsoRunState =>
        val chk = rs.runCache.get("test" -> "l")
        assertResult(None)(chk)
      case _ => fail("Returned Invalid State")}}
}

class LoadBindingsHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = LoadBindingsHandler(eio, efi)
  
  "LoadBindingsHandler" should "correctly load bindings from provided file using -f option" in {
    val (state, _) = runWithArgs()()("f" -> "testBindings.json")
    state match{
      case rs: EsoRunState =>
        val chk = rs.binds.get("test")
        assertResult(Some("testCommand"))(chk)}}
}

class SaveBindingsHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = SaveBindingsHandler(efi)
  
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

class ListBindingsHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = ListBindingsHandler(eio)
  
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

class SetVarHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = SetVarHandler(eio)
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

class SetDefaultsHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = SetDefaultsHandler
  
  "SetDefaultHandler" should "reset the state to defaults" in {
    val state = EsoRunState(immutable.HashMap(), immutable.HashMap(), immutable.HashMap(), immutable.HashMap(), immutable.HashMap(), immutable.HashMap(), immutable.HashMap(), immutable.HashMap())
    val (res, _) = runStateWithArgs()(state)()
    assertResult(EsoRunState.default)(res)}
}

class ListLangsHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = ListLangsHandler(eio)
  
  val ref: String =
    """Languages...
      |- ///
      |- ALPL
      |- Befunge-93
      |- Befunge-98
      |- BrainFuck
      |- Deadfish
      |- Emmental
      |- FracTran
      |- FracTran++
      |- Glypho
      |- Grass
      |- LazyBird
      |- LazyK
      |- Metatape
      |- NULL
      |- P''
      |- PATH
      |- Platts
      |- Prelude
      |- SNUSP
      |- Scala
      |- Thue
      |- Unlambda
      |- Volatile
      |- WhiteSpace
      |- Wierd
      |- WordLang
      |
      |Translators...
      |- FlufflePuff <=> BrainFuck
      |- GlyphoShorthand <=> Glypho
      |- LazyK_CC <=> LazyK
      |- LazyK_Iota <=> LazyK
      |- LazyK_Jot <=> LazyK
      |- LazyK_Unlambda <=> LazyK
      |- Ook <=> BrainFuck
      |- WSAssembly <=> WhiteSpace
      |
      |Transpilers...
      |- BrainFuck => C++
      |- BrainFuck => LazyK
      |- BrainFuck => Metatape
      |- BrainFuck => Prelude
      |- BrainFuck => SNUSP
      |- BrainFuck => Scala
      |- Lambda_Calculus => LazyK_Unlambda
      |- Lambda_Calculus => Unlambda
      |- WhiteSpace => Scala
      |
      |""".stripMargin
  
  "ListLangsHandler" should "correctly list the default language components" in {
    val (_, res) = runWithArgs()()()
    assertResult(normLines(ref))(normLines(res))}
}

class listVarsHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = ListVarsHandler(eio)
  
  val ref: String =
    """Runtime Parameters...
      |- appendInp     	= false	(append console input to the end of file input (useful for some self-interpreters))
      |- bfDiv         	= true 	(toggle whether or not divison by 0 evaluates to 0 in Befunge-98)
      |- bfRetCode     	= false	(toggle whether or not the Befunge-98 return code is displayed)
      |- cache         	= true 	(cache initialized state of programs for faster repeated loading)
      |- debug         	= false	(toggle debug information for the interface and languages that support it)
      |- dfChar        	= true 	(toggle whether or not to print Deadfish output as char values)
      |- dyn           	= false	(resize tape as needed for BF interpreter to eliminate memory limitations)
      |- echoFileInp   	= false	(print file input to the console as it is used, makes it look as if the input was entered into the console directly)
      |- fPtr          	= true 	(toggle whether output for P'' programs starts at the read head going right or at the end of the tape going left)
      |- indent        	= false	(toggle whether or not to neatly indent generated Scala code)
      |- log           	= false	(toggle detailed console logging)
      |- normLineBreaks	= true 	(normalize all line breaks to '\n' when reading source files (for instance, '\r\n' => '\n'))
      |- pNull         	= false	(toggle whether to print the null/empty character in the output of P'' programs)
      |- preludePar    	= false	(run Prelude voices in parallel, can speed up execution of some programs)
      |- printNum      	= false	(print output as numerical values rather than characters)
      |- sHead         	= true 	(toggle whether the read head starts at the beginning of the initial tape or the right end of the tape for P'')
      |- time          	= false	(print program duration on completion)
      |- bfOpt         	= 2    	(BrainFuck interpreter selection: 0=base, 1=optimized, 2=compiled)
      |- charWidth     	= 8    	(bit width of input characters for languages that do bitwise I/O)
      |- fileEOF       	= 0    	(character value to end file input strings with)
      |- init          	= 40000	(initial tape size for interpreters with a data tape)
      |- methSize      	= 1000 	(maximum number of blocks in a generated method (for compiling interpreters)
      |- olen          	= -1   	(maximum output length, useful for non-terminating programs, -1=infinite)
      |
      |""".stripMargin
  
  "listVarsHandler" should "correctly list the default runtime parameters" in {
    val (_, res) = runWithArgs()()()
    assertResult(normLines(ref))(normLines(res))}
}

class listFileAssociationsHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = ListFileAssociationsHandler(eio)
  
  val ref: String =
    """|File Associations...
       |- .alpl => ALPL
       |- .b => BrainFuck
       |- .b93 => Befunge-93
       |- .b98 => Befunge-98
       |- .cpp => C++
       |- .df => Deadfish
       |- .emm => Emmental
       |- .fl => FlufflePuff
       |- .ft => FracTran
       |- .ftp => FracTran++
       |- .glo => Glypho
       |- .glos => GlyphoShorthand
       |- .grs => Grass
       |- .lazy => LazyK
       |- .lzb => LazyBird
       |- .mt => Metatape
       |- .nul => NULL
       |- .ook => Ook
       |- .path => PATH
       |- .pdp => P''
       |- .pld => Prelude
       |- .plts => Platts
       |- .scala => Scala
       |- .slash => ///
       |- .snusp => SNUSP
       |- .th => Thue
       |- .unl => Unlambda
       |- .vol => Volatile
       |- .wd => Wierd
       |- .wl => WordLang
       |- .ws => WhiteSpace
       |- .wsa => WSAssembly
       |
       |""".stripMargin
  
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

class SaveFileAssociationsHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = SaveFileAssociationsHandler(efi)
  
  val refStr: String = """{"alpl":"ALPL","b":"BrainFuck","b93":"Befunge-93","b98":"Befunge-98","cpp":"C++","df":"Deadfish","emm":"Emmental","fl":"FlufflePuff","ft":"FracTran","ftp":"FracTran++","glo":"Glypho","glos":"GlyphoShorthand","grs":"Grass","lazy":"LazyK","lzb":"LazyBird","mt":"Metatape","names":["ftp","lazy","slash","th","df","b98","wl","b93","ws","pld","plts","grs","path","mt","pdp","cpp","lzb","glo","nul","glos","b","emm","fl","vol","alpl","unl","scala","ft","ook","wd","wsa","snusp"],"nul":"NULL","ook":"Ook","path":"PATH","pdp":"P''","pld":"Prelude","plts":"Platts","scala":"Scala","slash":"///","snusp":"SNUSP","th":"Thue","unl":"Unlambda","vol":"Volatile","wd":"Wierd","wl":"WordLang","ws":"WhiteSpace","wsa":"WSAssembly"}"""
  
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

class LoadFileAssociationsHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = LoadFileAssociationsHandler(eio, efi)
  
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
}

class AddFileAssociationHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = AddFileAssociationHandler
  
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

class DropFileAssociationHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = DropFileAssociationHandler
  
  "DropFileAssociationHandler" should "drop individual file associations" in {
    val assoc = immutable.HashMap("e1" -> "l1", "e2" -> "l2")
    val state = EsoRunState.withItems(fileAssoc = assoc)
    runStateWithArgs()(state)("e" -> "e1") match{
      case (rs: EsoRunState, _) =>
        assert(!rs.fileAssoc.isDefinedAt("e1"))
        assert(rs.fileAssoc.isDefinedAt("e2"))
      case _ => fail("Unexpected Halt State")}}
}

class ClearFileAssociationsHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = ClearFileAssociationsHandler
  
  "ClearFileAssociationsHandler" should "clear all file associations" in {
    val assoc = immutable.HashMap("e1" -> "l1", "e2" -> "l2")
    val state = EsoRunState.withItems(fileAssoc = assoc)
    runStateWithArgs()(state)() match{
      case (rs: EsoRunState, _) => assert(rs.fileAssoc.isEmpty)
      case _ => fail("Unexpected Halt State")}}
}

class ExitHandlerSpec extends InterfaceHandlerSpec{
  def currentHandler(eio: EsoTestInterface, efi: EsoFileInterface): InterfaceHandler = ExitHandler
  
  "ExitHandler" should "return the halt state" in {
    val (res, _) = runWithArgs()()()
    assertResult(EsoHalt)(res)}
}