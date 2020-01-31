package ui

import java.io.{File, PrintWriter}

import brainfuck.{BFTranslator, GenBFT}
import common.{DoOrErr, DoOrNull, DoOrOp, Done, EsoExcep, EsoObj, TimeIt, Trampoline}
import org.typelevel.jawn.Parser
import org.typelevel.jawn.ast.{JArray, JNull, JObject, JString, JValue}

import scala.collection.immutable
import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

abstract class InterfaceHandler extends EsoObj{
  val nam: String
  val helpStr: String
  
  def apply(state: EsoRunState)(args: immutable.HashMap[String, String]): EsoState
  
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
  
  def writeFile(fnam: String, str: String): Unit = {
    val oFile = new PrintWriter(new File(fnam))
    oFile.print(str)
    oFile.close()}
}

case class RunProgHandler(eio: EsoIOInterface = EsoConsoleInterface) extends InterfaceHandler{
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
        val fStr = EsoFileReader.readFile(fnam) map (s => (s + state.nums("fileEOF").toChar).to(LazyList).map{c => if(echoFInp) eio.print(c); c})
        if(appFlg) fStr map (s => s :++ LazyList.continually(eio.readLine + '\n').flatten)
        else fStr
      case None => Success(LazyList.continually(eio.readLine + '\n').flatten)}
    
    def printer(out: Seq[Char]): Unit = args.get("o") match{
      case Some(onam) =>
        val of = new PrintWriter(new File(onam))
        out foreach{c =>
          val str = if(printNum) c.toInt.toString + ' ' else c.toString
          eio.print(str)
          of.print(str)
          of.flush()}
        of.close()
      case None => out foreach(c => eio.print(if(printNum) c.toInt.toString + ' ' else c.toString))}
    
    Trampoline.doOrElse(state){
      DoOrOp(args.get("s"), "Missing Source File", eio){src =>
        DoOrOp(getLang(args, "l", "s"), "Unrecognized Language or File Extension", eio){lang =>
          if(logFlg) eio.print("Searching for translator path... ")
          DoOrOp(findTranslator(state, lang, state.interpNames), "Language Not Recognized", eio){
            case (inam, t) =>
              if(logFlg) eio.print("Done.\nRetrieving program from file... ")
              DoOrErr(EsoFileReader.readFile(src), eio){progRaw =>
                if(logFlg) eio.print(s"Done.\nTranslating program... ")
                DoOrErr(t(progRaw), eio){prog =>
                  if(logFlg) eio.print("Done.\nInitializing interpreter... ")
                  TimeIt(state.interps(inam)(state.config)(prog)) match{
                    case (i, dur) =>
                      if(logFlg) eio.println(s"Done in ${dur}ms.")
                      DoOrErr(i, eio){r =>
                        DoOrErr(inputs, eio){inp =>
                          TimeIt{tryAll{printer(olim(r(inp)))}} match{
                            case (flg, rdr) => flg match{
                              case Failure(e) =>
                                if(timeFlg) eio.println(s"\nError: $e\nProgram failed in ${rdr}ms")
                                else eio.println(s"\nError: $e")
                              case Success(_) =>
                                if(timeFlg) eio.println(s"\nProgram completed in ${rdr}ms")
                                else eio.println()}}
                          Done{state}}}}}}}}}}}
}

case class TranslateHandler(eio: EsoIOInterface = EsoConsoleInterface) extends InterfaceHandler{
  val nam: String = "translate"
  val helpStr: String = "<-s :sourceFileName:> (-tl :targetLanguage, -o :targetFileName:) {-sl :sourceLanguage}"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val logFlg = state.bools("log")
    
    def printer(str: String): Unit = args.get("o") match{
      case Some(onam) =>
        writeFile(onam, str)
        if(logFlg) eio.println(s"Translation saved to $onam.")
      case None => eio.println(str)}
    
    Trampoline.doOrElse(state){
      DoOrOp(args.get("s"), "Not Enough Arguments", eio){i =>
        DoOrOp(getLang(args, "sl", "s"), "Unrecognized Source Language or File Extension", eio){sl =>
          DoOrOp(getLang(args, "tl", "o"), "Unrecognized Target Language or File Extension", eio){tl =>
            if(logFlg) eio.print("Searching for translation path... ")
            DoOrOp(buildTrans(state)(sl, tl), "No Applicable Translation Path", eio){t =>
              if(logFlg) eio.print("Done.\nRetrieving program from file... ")
              DoOrErr(EsoFileReader.readFile(i), eio){progRaw =>
                if(logFlg) eio.print("Done.\nTranslating... ")
                DoOrErr(t(progRaw), eio){prog =>
                  if(logFlg) eio.println("Done.")
                  printer(prog)
                  Done{state}}}}}}}}}
}

case class TranspileHandler(eio: EsoIOInterface = EsoConsoleInterface) extends InterfaceHandler{
  val nam: String = "transpile"
  val helpStr: String = "<-s :sourceFileName:> (-tl :targetLanguage, -o :targetFileName:) {-sl :sourceLanguage}"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val logFlg = state.bools("log")
    
    def printer(str: String): Unit = args.get("o") match{
      case Some(onam) =>
        writeFile(onam, str)
        if(logFlg) eio.println(s"Transpiled program saved to $onam.")
      case None => eio.println(str)}
    
    Trampoline.doOrElse(state){
      DoOrOp(args.get("s"), "Not Enough Arguments", eio){s =>
        DoOrOp(getLang(args, "sl", "s"), "Unrecognized Source Language or File Extension", eio){sl =>
          DoOrOp(getLang(args, "tl", "o"), "Unrecognized Target Language or File Extension", eio){tl =>
            if(logFlg) eio.print("Searching for source translator path... ")
            DoOrOp(findTranslator(state, sl, state.genNames.map(_._1)), "No Applicable Translation Path", eio){
              case (lin, tin) =>
                if(logFlg) eio.print("Done.\nSearching for target translator path... ")
                DoOrOp(findTranslator(state, state.genLinks(lin), tl), "No Applicable Translation Path", eio){
                  case (lout, tout) =>
                    if(logFlg) eio.print("Done.\nRetrieving program from file... ")
                    DoOrErr(EsoFileReader.readFile(s), eio){progRaw =>
                      if(logFlg) eio.print("Done.\nTranslating from source... ")
                      DoOrErr(tin(progRaw), eio){prog1 =>
                        if(logFlg) eio.print("Done.\nTranspiling... ")
                        TimeIt(state.gens((lin, lout))(state.config)(prog1)) match{
                          case (transTry, dur) =>
                            DoOrErr(transTry, eio){prog2 =>
                              if(logFlg) eio.print(s"Done in ${dur}ms.\nTranslating to target... ")
                              DoOrErr(tout(prog2), eio){prog3 =>
                                if(logFlg) eio.println("Done.")
                                printer(prog3)
                                Done{state}}}}}}}}}}}}
    
    state}
}

case class DefineBFLangHandler(eio: EsoIOInterface = EsoConsoleInterface) extends InterfaceHandler{
  val nam: String = "defineBFLang"
  val helpStr: String = ""
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = eio.readLine("Name: ") match{
    case "names" =>
      eio.println("""Error: Name Cannot Be "names"""")
      state
    case nam =>
      val syn = "[]<>+-,.".toVector map{c => (c.toString, eio.readLine(s"$c => "))}
      state.addTrans(GenBFT(nam, syn))}
}

object LoadBFLangsHandler extends InterfaceHandler{
  val nam: String = "loadBFLangs"
  val helpStr: String = "{-f :fileName:}"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val logFlg = state.bools("log")
    
    if(logFlg) print("Retrieving lang file... ")
    Trampoline.doOrElse(state){
      DoOrErr(EsoFileReader.readFile(args.getOrElse("f", EsoDefaults.defBFLFile))){langFile =>
        if(logFlg) print("Done.\nParsing file... ")
        DoOrErr(Parser.parseFromString[JValue](langFile)){jsv =>
          DoOrNull(jsv, "Parsed To Null"){jso =>
            if(logFlg) print(s"Done.\nBuilding translators... ")
            DoOrNull(jso.get("names"), "Missing Names Array"){namsJS =>
              Done{
                val tv = LazyList.from(0)
                  .map(i => namsJS.get(i))
                  .takeWhile(_ != JNull)
                  .map(_.asString)
                  .map(k => GenBFT(k, jso.get(k)))
                  .toVector
                println(s"Done.\nLoaded BF Langs:\n${tv.map(t => s"- ${t.name}").mkString("\n")}")
                state.addAllTrans(tv)}}}}}}}
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
    Trampoline.doOrElse(state){
      DoOrErr(EsoFileReader.readFile(fnam)){str =>
        DoOrErr(Parser.parseFromString[JValue](str)){jVal =>
          DoOrNull(jVal.get("names"), "Missing Name List"){jNams =>
            Done{
              LazyList.from(0)
                .map(jNams.get)
                .takeWhile(_ != JNull)
                .map(_.asString)
                .map(k => (k, jVal.get(k).asString))
                .foldLeft(state){
                  case (s, (t, b)) => s.addBind(t, b)}}}}}}}
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
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = args.toVector.foldLeft(state){
    case (s, (k, v)) => s.setVar(k, v) match{
      case Success(ns) => ns
      case Failure(e) =>
        val str = e match{
          case EsoExcep(s) => s
          case _ => e.toString}
        println(s"Error: $str")
        s}}
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