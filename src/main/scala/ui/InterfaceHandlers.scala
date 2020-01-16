package ui

import java.io.{File, PrintWriter}

import brainfuck.{BFTranslator, GenBFT}
import common.{EsoExcep, EsoObj}
import org.typelevel.jawn.Parser
import org.typelevel.jawn.ast.{JArray, JNull, JObject, JString, JValue}

import scala.collection.immutable
import scala.collection.immutable.HashMap
import scala.io.{Source, StdIn}
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

trait InterfaceHandler extends EsoObj{
  val nam: String
  val helpStr: String
  
  def apply(state: EsoRunState)(args: immutable.HashMap[String, String]): EsoState
  
  val encodings: LazyList[String] = LazyList("UTF-8", "Cp1252", "UTF-16")
  val fextReg: Regex = raw""".*\.(\w+)\z""".r
  
  def findTranslator(state: EsoRunState, sl: String, tls: Seq[String]): Option[(String, String => Try[String])] = findTranslator(state, Seq(sl), tls) map{case (_, tl, t) => (tl, t)}
  def findTranslator(state: EsoRunState, sls: Seq[String], tl: String): Option[(String, String => Try[String])] = findTranslator(state, sls, Seq(tl)) map{case (sl, _, t) => (sl, t)}
  def findTranslator(state: EsoRunState, sls: Seq[String], tls: Seq[String]): Option[(String, String, String => Try[String])] = {
    sls.iterator.map{sl =>
      tls.iterator.map{tl =>
        (tl, buildTrans(state)(sl, tl))}
        .collectFirst{case (tl, Some(t)) => (sl, tl, t)}}
      .collectFirst{case Some(t) => t}}
  
  def buildTrans(state: EsoRunState)(sl: String, tl: String): Option[String => Try[String]] = {
    val links = state.transLinks
    val seed: String => Try[String] = str => Success(str)
    def compChain(vec: Vector[String]): String => Try[String] = vec.sliding(2)
      .map{case l0 +: l1 +: _ => state.getTrans(l0, l1)}
      .collect{case Some(t) => t(state.config).apply(_)}
      .foldLeft(seed){case (ac, t) => (str: String) => ac(str) flatMap t}
    if(sl == tl) Some(seed)
    else Iterator
      .iterate(Vector(Vector(sl))){vec =>
        vec.flatMap{c =>
          val nxt = links.get(c.last) match{
            case Some(v) => v
            case _ => Vector()}
          nxt.filter(b => !c.contains(b)).map(l => c :+ l)}}
      .takeWhile(_.nonEmpty)
      .flatten
      .collectFirst{case vec if vec.last == tl => compChain(vec)}}
  
  def getLang(args: immutable.HashMap[String, String], a: String, f: String): Option[String] = args.get(a) match{
    case None => args.get(f) match{
      case Some(fextReg(fext)) => EsoDefaults.fileExtensionMap.get(fext)
      case _ => None}
    case lop => lop}
  
  def getSource(fnam: String): Option[Try[String]] = encodings
    .map(e => readFile(fnam, e))
    .collectFirst {
      case s: Success[String] => s
      case Failure(ex: java.io.FileNotFoundException) => Failure(ex)}
  
  def readFile(fnam: String): Try[String] = getSource(fnam) match{
    case Some(s) => s
    case None => Failure(EsoExcep("Incompatible File Encoding"))}
  def readFile(fnam: String, enc: String): Try[String] = Try{
    val src = Source.fromFile(fnam, enc)
    val res = src.mkString.replaceAllLiterally("\r\n", "\n")
    src.close()
    res}
  
  def writeFile(fnam: String, str: String): Unit = {
    val oFile = new PrintWriter(new File(fnam))
    oFile.print(str)
    oFile.close()}
  
  def doOrErr[A, B](inp: Try[A])(act: A => B): Option[B] = inp match{
    case Success(i) => Some(act(i))
    case Failure(e) =>
      e match{
        case EsoExcep(info) => println(s"Error: common.EsoExcep ($info)")
        case _ => println(s"Error: $e")}
      None}
  def doOrOp[A, B](inp: Option[A], err: String)(act: A => B): Option[B] = inp match{
    case Some(_) => inp map act
    case None =>
      println(s"Error: $err")
      None}
  def doOrNull[B](inp: JValue, err: String)(act: JValue => B): Option[B] = inp match{
    case JNull =>
      println(s"Error: $err")
      None
    case _ => Some(act(inp))}
  def doOrElse[A](default: A)(act: => Option[A]): A = act match{
    case Some(x) => x
    case None => default}
  
  def timeIt[T](thing: => T): (T, Long) = {
    val t = System.currentTimeMillis
    val res = thing
    (res, System.currentTimeMillis - t)}
}

object DebugHandler extends InterfaceHandler{
  val nam: String = "debug"
  val helpStr: String = "<-s :sourceFileName:> {-l :language:, -i :inputFileName:, -o :outputFileName:}"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val printNum = state.bools("printNum")
    val logFlg = state.bools("log")
    val timeFlg = state.bools("time")
    val appFlg = state.bools("appendInp")
    val echoFInp = state.bools("echoFileInp")
    
    def olim(res: LazyList[Char]): LazyList[Char] = state.nums("olen") match{
      case -1 => res
      case n => res.take(n)}
    
    def inputs: Try[LazyList[Char]] = args.get("i") match{
      case Some(fnam) =>
        val fStr = readFile(fnam) map (s => (s + state.nums("fileEOF").toChar).to(LazyList).map{c => if(echoFInp) print(c); c})
        if(appFlg) fStr map (s => s :++ LazyList.continually(StdIn.readLine + '\n').flatten)
        else fStr
      case None => Success(LazyList.continually(StdIn.readLine + '\n').flatten)}
    
    def printer(out: Seq[Char]): Unit = args.get("o") match{
      case Some(onam) =>
        val of = new PrintWriter(new File(onam))
        out foreach{c =>
          val str = if(printNum) c.toInt.toString + ' ' else c.toString
          print(str)
          of.print(str)
          of.flush()}
        of.close()
      case None => out foreach(c => print(if(printNum) c.toInt.toString + ' ' else c.toString))}
    
    doOrOp(args.get("s"), "Missing Source File"){src =>
      doOrOp(getLang(args, "l", "s"), "Unrecognized Language or File Extension"){lang =>
        if(logFlg) print("Searching for translator path... ")
        doOrOp(findTranslator(state, lang, state.interpNames), "Language Not Recognized"){
          case (inam, t) =>
            if(logFlg) print("Done.\nRetrieving program from file... ")
            doOrErr(readFile(src)){progRaw =>
              if(logFlg) print(s"Done.\nTranslating program... ")
              doOrErr(t(progRaw)){prog =>
                if(logFlg) print("Done.\nInitializing interpreter... ")
                timeIt(state.interps(inam)(state.config)(prog)) match{
                  case (i, dur) =>
                    if(logFlg) println(s"Done in ${dur}ms.\nRunning program...")
                    doOrErr(i){r =>
                      doOrErr(inputs){inp =>
                        printer(olim(r(inp)))
                        println("\nProgram Completed")}}}}}}}}
    
    state}
}

object RunProgHandler extends InterfaceHandler{
  val nam: String = "run"
  val helpStr: String = "<-s :sourceFileName:> {-l :language:, -i :inputFileName:, -o :outputFileName:}"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val printNum = state.bools("printNum")
    val logFlg = state.bools("log")
    val timeFlg = state.bools("time")
    val appFlg = state.bools("appendInp")
    val echoFInp = state.bools("echoFileInp")
    
    def olim(res: LazyList[Char]): LazyList[Char] = state.nums("olen") match{
      case -1 => res
      case n => res.take(n)}
    
    def inputs: Try[LazyList[Char]] = args.get("i") match{
      case Some(fnam) =>
        val fStr = readFile(fnam) map (s => (s + state.nums("fileEOF").toChar).to(LazyList).map{c => if(echoFInp) print(c); c})
        if(appFlg) fStr map (s => s :++ LazyList.continually(StdIn.readLine + '\n').flatten)
        else fStr
      case None => Success(LazyList.continually(StdIn.readLine + '\n').flatten)}
    
    def printer(out: Seq[Char]): Unit = args.get("o") match{
      case Some(onam) =>
        val of = new PrintWriter(new File(onam))
        out foreach{c =>
          val str = if(printNum) c.toInt.toString + ' ' else c.toString
          print(str)
          of.print(str)
          of.flush()}
        of.close()
      case None => out foreach(c => print(if(printNum) c.toInt.toString + ' ' else c.toString))}
    
    doOrOp(args.get("s"), "Missing Source File"){src =>
      doOrOp(getLang(args, "l", "s"), "Unrecognized Language or File Extension"){lang =>
        if(logFlg) print("Searching for translator path... ")
        doOrOp(findTranslator(state, lang, state.interpNames), "Language Not Recognized"){
          case (inam, t) =>
            if(logFlg) print("Done.\nRetrieving program from file... ")
            doOrErr(readFile(src)){progRaw =>
              if(logFlg) print(s"Done.\nTranslating program... ")
              doOrErr(t(progRaw)){prog =>
                if(logFlg) print("Done.\nInitializing interpreter... ")
                timeIt(state.interps(inam)(state.config)(prog)) match{
                  case (i, dur) =>
                    if(logFlg) println(s"Done in ${dur}ms.\nRunning program...")
                    doOrErr(i){r =>
                      doOrErr(inputs){inp =>
                        timeIt{tryAll{printer(olim(r(inp)))}} match{
                          case (flg, rdr) => flg match{
                            case Failure(e) =>
                              if(timeFlg) println(s"\nError: $e\nProgram failed in ${rdr}ms")
                              else println(s"\nError: $e")
                            case Success(_) =>
                              if(timeFlg) println(s"\nProgram completed in ${rdr}ms")
                              else println}}}}}}}}}}
    
    state}
}

object TranslateHandler extends InterfaceHandler{
  val nam: String = "translate"
  val helpStr: String = "<-s :sourceFileName:> (-tl :targetLanguage, -o :targetFileName:) {-sl :sourceLanguage}"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val logFlg = state.bools("log")
    
    def printer(str: String): Unit = args.get("o") match{
      case Some(onam) =>
        writeFile(onam, str)
        if(logFlg) println(s"Translation saved to $onam.")
      case None => println(str)}
    
    args.get("s") match{
      case None => println("Error: Not Enough Arguments")
      case Some(i) =>
        doOrOp(getLang(args, "sl", "s"), "Unrecognized Language or File Extension"){sl =>
          doOrOp(getLang(args, "tl", "o"), "Unrecognized Target Language or File Extension"){tl =>
            if(logFlg) print("Searching for translation path... ")
            doOrOp(buildTrans(state)(sl, tl), "No Applicable Translation Path"){t =>
              if(logFlg) print("Done.\nRetrieving program from file... ")
              doOrErr(readFile(i)){progRaw =>
                if(logFlg) print("Done.\nTranslating... ")
                doOrErr(t(progRaw)){prog =>
                  if(logFlg) println("Done.")
                  printer(prog)}}}}}}
    
    state}
}

object TranspileHandler extends InterfaceHandler{
  val nam: String = "transpile"
  val helpStr: String = "<-s :sourceFileName:> (-tl :targetLanguage, -o :targetFileName:) {-sl :sourceLanguage}"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val logFlg = state.bools("log")
    
    def printer(str: String): Unit = args.get("o") match{
      case Some(onam) =>
        writeFile(onam, str)
        if(logFlg) println(s"Transpiled program saved to $onam.")
      case None => println(str)}
    
    args.get("s") match{
      case None => println("Error: Not Enough Arguments")
      case Some(s) =>
        doOrOp(getLang(args, "sl", "s"), "Unrecognized Source Language or File Extension"){sl =>
          doOrOp(getLang(args, "tl", "o"), "Unrecognized Target Language or File Extension"){tl =>
            if(logFlg) print("Searching for source translator path... ")
            doOrOp(findTranslator(state, sl, state.genNames.map(_._1)), "No Applicable Translation Path"){
              case (lin, tin) =>
                if(logFlg) print("Done.\nSearching for target translator path... ")
                doOrOp(findTranslator(state, state.genLinks(lin), tl), "No Applicable Translation Path"){
                  case (lout, tout) =>
                    if(logFlg) print("Done.\nRetrieving program from file... ")
                    doOrErr(readFile(s)){progRaw =>
                      if(logFlg) print("Done.\nTranslating from source... ")
                      doOrErr(tin(progRaw)){prog1 =>
                        if(logFlg) print("Done.\nTranspiling... ")
                        timeIt(state.gens((lin, lout))(state.config)(prog1)) match{
                          case (transTry, dur) =>
                            doOrErr(transTry){prog2 =>
                              if(logFlg) print(s"Done in ${dur}ms.\nTranslating to target... ")
                              doOrErr(tout(prog2)){prog3 =>
                                if(logFlg) println("Done.")
                                printer(prog3)}}}}}}}}}}
    
    state}
}

object DefineBFLangHandler extends InterfaceHandler{
  val nam: String = "defineBFLang"
  val helpStr: String = ""
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = StdIn.readLine("Name: ") match{
    case "names" =>
      println("""Error: Name Cannot Be "names"""")
      state
    case nam =>
      val syn = "[]<>+-,.".toVector map{c => (c.toString, StdIn.readLine(s"$c => "))}
      state.addTrans(GenBFT(nam, syn))}
}

object LoadBFLangsHandler extends InterfaceHandler{
  val nam: String = "loadBFLangs"
  val helpStr: String = "{-f :fileName:}"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val logFlg = state.bools("log")
    
    if(logFlg) print("Retrieving lang file... ")
    doOrElse(state){
      doOrErr(readFile(args.getOrElse("f", EsoDefaults.defBFLFile))){langFile =>
        if(logFlg) print("Done.\nParsing file... ")
        doOrErr(Parser.parseFromString[JValue](langFile)){jsv =>
          doOrNull(jsv, "Parsed To Null"){jso =>
            if(logFlg) print(s"Done.\nBuilding translators... ")
            doOrNull(jso.get("names"), "Missing Names Array"){namsJS =>
              val tv = LazyList.from(0)
                .map(i => namsJS.get(i))
                .takeWhile(_ != JNull)
                .map(_.asString)
                .map(k => GenBFT(k, jso.get(k)))
                .toVector
              println(s"Done.\nLoaded BF Langs:\n${tv.map(t => s"- ${t.name}").mkString("\n")}")
              state.addAllTrans(tv)}}}}.flatten.flatten.flatten}}
}

object SaveBFLangsHandler extends InterfaceHandler{
  val nam: String = "saveBFLangs"
  val helpStr: String = "{-f :fileName:}"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val fnam = args.getOrElse("f", EsoDefaults.defBFLFile)
    val ignore = EsoDefaults.defTransVec.map(_.id)
    val jsoVec = state.trans.toVector.collect{
      case (id, bft: BFTranslator) if !ignore.contains(id) => (bft.name, bft.toJObject)}
    val namVec = jsoVec.map{case (k, _) => JString(k)}
    val jsObj = JObject.fromSeq(jsoVec :+ ("names", JArray.fromSeq(namVec)))
    writeFile(fnam, jsObj.render)
    if(state.bools("log")) println(s"Translators saved to $fnam.")
    
    state}
}

object ShowSyntaxHandler extends InterfaceHandler{
  val nam: String = "syntax"
  val helpStr: String = "{-l :language:}"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val str = args.get("l") match{
      case Some(lang) => state.trans.toVector.collectFirst{
        case (_, t: BFTranslator) if t.name == lang => t} match{
        case Some(t) => s"Syntax for $lang...\n${t.kvPairs.map{case (k, v) => s"$k: $v"}.mkString("\n")}\n"
        case None => "Error: Language Not Recognized"}
      case _ => "Error: Not Enough Arguments"}
    println(str)
    
    state}
}

object ClearBindingsHandler extends InterfaceHandler{
  val nam: String = "clrBindings"
  val helpStr: String = ""
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = state.clearBinds
}

object LoadBindingsHandler extends InterfaceHandler{
  val nam: String = "loadBindings"
  val helpStr: String = "{-f :fileName:}"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val fnam = args.getOrElse("f", EsoDefaults.defBindFile)
    doOrElse(state){
      doOrErr(readFile(fnam)){str =>
        doOrErr(Parser.parseFromString[JValue](str)){jVal =>
          doOrNull(jVal.get("names"), "Missing Name List"){jNams =>
            LazyList.from(0)
              .map(jNams.get)
              .takeWhile(_ != JNull)
              .map(_.asString)
              .map(k => (k, jVal.get(k).asString))
              .foldLeft(state){
                case (s, (t, b)) => s.addBind(t, b)}}}}.flatten.flatten}}
}

object SaveBindingsHandler extends InterfaceHandler{
  val nam: String = "saveBindings"
  val helpStr: String = "{-f :fileName:}"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val fnam = args.getOrElse("f", "userBindings.json")
    val bindVec = state.binds.toVector
    val nams = bindVec.map{case (k, _) => JString(k)}
    val jsoPairs = bindVec.map{case (k, v) => (k, JString(v))} :+ ("names", JArray.fromSeq(nams))
    val bindStr = JObject.fromSeq(jsoPairs).render
    writeFile(fnam, bindStr)
    
    state}
}

object ListBindingsHandler extends InterfaceHandler{
  val nam: String = "listBindings"
  val helpStr: String = ""
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val bindStr = state.binds.toVector
      .map{case (k, v) => s"- $k => $v"}
      .sorted
      .mkString("\n")
    
    val str =
      s"""|Bindings...
          |$bindStr
          |""".stripMargin
    println(str)
    
    state}
}

object SetVarHandler extends InterfaceHandler{
  val nam: String = "set"
  val helpStr: String = "{-:varName: :value:}*"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = args.toVector.foldLeft(state){case (s, (k, v)) => s.setVar(k, v)}
}

object SetDefaultsHandler extends InterfaceHandler{
  val nam: String = "defaults"
  val helpStr: String = ""
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = EsoRunState.default
}

object ListLangsHandler extends InterfaceHandler{
  val nam: String = "listLangs"
  val helpStr: String = ""
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val str =
      f"""|Languages...
          |${state.interps.values.map(i => s"- $i").toVector.sorted.mkString("\n")}
          |
          |Translators...
          |${state.trans.values.map(t => s"- $t").toVector.sorted.mkString("\n")}
          |
          |Transpilers...
          |${state.gens.keys.map{case (snam, dnam) => s"- $snam => $dnam"}.toVector.sorted.mkString("\n")}
          |""".stripMargin
    println(str)
    
    state}
}

object ListVarsHandler extends InterfaceHandler{
  val nam: String = "listVars"
  val helpStr: String = ""
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val boolVec = state.bools.toVector.sortBy(_._1)
    val numVec = state.nums.toVector.sortBy(_._1)
    val pairs = (boolVec ++ numVec).map{case (id, d) => (id, d.toString)}
    val namLen = pairs.map(_._1.length).max
    val datLen = pairs.map(_._2.length).max
    val strs = pairs.map{case (id, d) => s"- %-${namLen}s\t= %-${datLen}s\t(${EsoDefaults.defDesc(id)})".format(id, d)}.mkString("\n")
    val str =
      s"""|Runtime Parameters...
          |$strs
          |""".stripMargin
    println(str)
    
    state}
}

object ListFileAssociationsHandler extends InterfaceHandler{
  val nam: String = "listFileAssociations"
  val helpStr: String = ""
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val str =
      s"""|File Associations...
          |${EsoDefaults.fileExtensionsVec.map{case (f, l) => s"- .$f => $l"}.mkString("\n")}
          |""".stripMargin
    println(str)
    state}
}

object ExitHandler extends InterfaceHandler{
  val nam: String = "exit"
  val helpStr: String = ""
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = EsoHalt
}