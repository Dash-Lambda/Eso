package ui

import languages.brainfuck.{BFTranslator, GenBFT}
import common.{DoOrErr, DoOrNull, DoOrOp, Done, EsoExcep, EsoObj, TimeIt, Trampoline}
import org.typelevel.jawn.Parser
import org.typelevel.jawn.ast.{JArray, JNull, JObject, JString, JValue}

import scala.collection.immutable
import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

abstract class CommandHandler extends EsoObj{
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
  
  def getLang(state: EsoRunState)(args: immutable.HashMap[String, String], a: String, f: String): Option[String] = args.get(a) match{
    case None => args.get(f) match{
      case Some(fextReg(fext)) => state.fileAssoc.get(fext)
      case _ => None}
    case lop => lop}
  
  def readStringMapJSON(efi: EsoFileInterface, eio: EsoIOInterface = EsoDummyInterface)(fnam: String): Try[Vector[(String, String)]] = Try{
    Trampoline.doOrElse(Vector[(String, String)]()){
      DoOrErr(efi.readFile(fnam), eio){str =>
        DoOrErr(Parser.parseFromString[JValue](str), eio){jVal =>
          DoOrNull(jVal.get("names"), "Missing Name List", eio){jNams =>
            Done{
              LazyList.from(0)
                .map(jNams.get)
                .takeWhile(_ != JNull)
                .map(_.asString)
                .map(k => (k, jVal.get(k).asString))
                .toVector}}}}}}
}

trait LoadHandler{
  def loadOnly(state: EsoRunState): EsoRunState
}

case class RunProgHandler(eio: EsoIOInterface = EsoConsoleInterface, efi: EsoFileInterface = SystemFileInterface) extends CommandHandler{
  val nam: String = "run"
  val helpStr: String = "Run a program from a source file (<-s :sourceFileName:> {-l :language:, -i :inputFileName:, -o :outputFileName:})"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val printNum = state.bools("printNum")
    val logFlg = state.bools("log")
    val timeFlg = state.bools("time")
    val appFlg = state.bools("appendInp")
    val echoFInp = state.bools("echoFileInp")
    val normLineFlag = state.bools("normLineBreaks")
    val cache = state.bools("cache")
    
    def olim(res: LazyList[Char]): LazyList[Char] = state.nums("olen") match{
      case -1 => res
      case n => res.take(n)}
    
    def inputs: Try[LazyList[Char]] = args.get("i") match{
      case Some(fnam) =>
        val fStr = efi.readFile(fnam, normLineFlag) map (s => (s + state.nums("fileEOF").toChar).to(LazyList).map{ c => if(echoFInp) eio.print(c); c})
        if(appFlg) fStr map (s => s :++ eio.charsIterator)
        else fStr
      case None => Success(eio.charsIterator.to(LazyList))}
    
    def printer(out: Seq[Char]): Unit = args.get("o") match{
      case Some(onam) =>
        efi.writeFile(onam, "")
        out foreach{c =>
          val str = if(printNum) c.toInt.toString + ' ' else c.toString
          eio.print(str)
          efi.appendFile(onam, str)}
      case None => out foreach(c => eio.print(if(printNum) c.toInt.toString + ' ' else c.toString))}
    
    def runWithRunner(runner: Try[Seq[Char] => LazyList[Char]], src: String, lang: String): DoOrErr[Seq[Char] => LazyList[Char], EsoRunState] = {
      DoOrErr(runner, eio){r =>
        DoOrErr(inputs, eio){inp =>
          TimeIt{tryAll{printer(olim(r(inp)))}} match{
            case (flg, rdr) => flg match{
              case Failure(e) =>
                if(timeFlg) eio.println(s"\nError: $e\nProgram failed in ${rdr}ms")
                else eio.println(s"\nError: $e")
              case Success(_) =>
                if(timeFlg) eio.println(s"\nProgram completed in ${rdr}ms")
                else eio.println()}}
          Done(if(cache) state.cacheFunc(src, lang, efi.getLastModified(src).get, r) else state)}}}
    
    Trampoline.doOrElse(state){
      DoOrOp(args.get("s"), "Missing Source File", eio){src =>
        DoOrOp(getLang(state)(args, "l", "s"), "Unrecognized Language or File Extension", eio){lang =>
          state.runCache.get((src, lang)) match{
            case Some((prg, lm)) if cache && lm == efi.getLastModified(src).get =>
              if(logFlg) eio.println("Using cached run (disable with 'set -cache off')")
              runWithRunner(Success(prg), src, lang)
            case _ =>
              if(logFlg) eio.print("Searching for translator path... ")
              DoOrOp(findTranslator(state, lang, state.interpNames), "Language Not Recognized", eio){
                case (inam, t) =>
                  if(logFlg) eio.print("Done.\nRetrieving program from file... ")
                  DoOrErr(efi.readFile(src, normLineFlag), eio){ progRaw =>
                    if(logFlg) eio.print(s"Done.\nTranslating program... ")
                    DoOrErr(t(progRaw), eio){prog =>
                      if(logFlg) eio.print("Done.\nInitializing interpreter... ")
                      TimeIt(state.interps(inam)(state.config)(prog)) match{
                        case (i, dur) =>
                          if(logFlg) {
                            if(timeFlg) eio.println(s"Done in ${dur}ms.")
                            else eio.println(s"Done.")}
                          runWithRunner(i, src, lang)}}}}}}}}}}

case class TranslateHandler(eio: EsoIOInterface = EsoConsoleInterface, efi: EsoFileInterface = SystemFileInterface) extends CommandHandler{
  val nam: String = "translate"
  val helpStr: String = "Translate a program from one language to another (<-s :sourceFileName:> [-tl :targetLanguage, -o :targetFileName:] {-sl :sourceLanguage})"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val logFlg = state.bools("log")
    
    def printer(str: String): Unit = args.get("o") match{
      case Some(onam) =>
        efi.writeFile(onam, str)
        if(logFlg) eio.println(s"Translation saved to $onam.")
      case None => eio.println(str)}
    
    Trampoline.doOrElse(state){
      DoOrOp(args.get("s"), "Not Enough Arguments", eio){i =>
        DoOrOp(getLang(state)(args, "sl", "s"), "Unrecognized Source Language or File Extension", eio){sl =>
          DoOrOp(getLang(state)(args, "tl", "o"), "Unrecognized Target Language or File Extension", eio){tl =>
            if(logFlg) eio.print("Searching for translation path... ")
            DoOrOp(buildTrans(state)(sl, tl), "No Applicable Translation Path", eio){t =>
              if(logFlg) eio.print("Done.\nRetrieving program from file... ")
              DoOrErr(efi.readFile(i), eio){ progRaw =>
                if(logFlg) eio.print("Done.\nTranslating... ")
                DoOrErr(t(progRaw), eio){prog =>
                  if(logFlg) eio.println("Done.")
                  printer(prog)
                  Done{state}}}}}}}}}
}

case class TranspileHandler(eio: EsoIOInterface = EsoConsoleInterface, efi: EsoFileInterface = SystemFileInterface) extends CommandHandler{
  val nam: String = "transpile"
  val helpStr: String = "Transpile a program from one language to another (<-s :sourceFileName:> [-tl :targetLanguage, -o :targetFileName:] {-sl :sourceLanguage})"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val logFlg = state.bools("log")
    
    def printer(str: String): Unit = args.get("o") match{
      case Some(onam) =>
        efi.writeFile(onam, str)
        if(logFlg) eio.println(s"Transpiled program saved to $onam.")
      case None => eio.println(str)}
    
    Trampoline.doOrElse(state){
      DoOrOp(args.get("s"), "Not Enough Arguments", eio){s =>
        DoOrOp(getLang(state)(args, "sl", "s"), "Unrecognized Source Language or File Extension", eio){sl =>
          DoOrOp(getLang(state)(args, "tl", "o"), "Unrecognized Target Language or File Extension", eio){tl =>
            if(logFlg) eio.print("Searching for source translator path... ")
            DoOrOp(findTranslator(state, sl, state.genNames.map(_._1)), "No Applicable Translation Path", eio){
              case (lin, tin) =>
                if(logFlg) eio.print("Done.\nSearching for target translator path... ")
                DoOrOp(findTranslator(state, state.genLinks(lin), tl), "No Applicable Translation Path", eio){
                  case (lout, tout) =>
                    if(logFlg) eio.print("Done.\nRetrieving program from file... ")
                    DoOrErr(efi.readFile(s), eio){ progRaw =>
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

case class DefineBFLangHandler(eio: EsoIOInterface = EsoConsoleInterface) extends CommandHandler{
  val nam: String = "defineBFLang"
  val helpStr: String = "Create a custom BrainFuck-derivative translator"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = eio.readLine("Name: ") match{
    case "names" =>
      eio.println("""Error: Name Cannot Be "names"""")
      state
    case nam =>
      val syn = "[]<>+-,.".toVector map{c => (c.toString, eio.readLine(s"$c => "))}
      state.addTrans(GenBFT(nam, syn))}
}

case class LoadBFLangsHandler(eio: EsoIOInterface = EsoConsoleInterface, efi: EsoFileInterface = SystemFileInterface) extends CommandHandler with LoadHandler{
  val nam: String = "loadBFLangs"
  val helpStr: String = "Load user-defined BrainFuck translators ({-f :fileName:})"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val logFlg = state.bools("log")
    
    if(logFlg) eio.print("Retrieving lang file... ")
    Trampoline.doOrElse(state){
      DoOrErr(efi.readFile(args.getOrElse("f", EsoDefaults.defBFLFile)), eio){ langFile =>
        if(logFlg) eio.print("Done.\nParsing file... ")
        DoOrErr(Parser.parseFromString[JValue](langFile), eio){jsv =>
          DoOrNull(jsv, "Parsed To Null", eio){jso =>
            if(logFlg) eio.print(s"Done.\nBuilding translators... ")
            DoOrNull(jso.get("names"), "Missing Names Array", eio){namsJS =>
              Done{
                val tv = LazyList.from(0)
                  .map(i => namsJS.get(i))
                  .takeWhile(_ != JNull)
                  .map(_.asString)
                  .map(k => GenBFT(k, jso.get(k)))
                  .toVector
                if(logFlg) eio.println(s"Done.")
                eio.println(s"Loaded BF Langs:\n${tv.map(t => s"- ${t.name}").mkString("\n")}")
                state.addAllTrans(tv)}}}}}}}
  
  def loadOnly(state: EsoRunState): EsoRunState = apply(state)(immutable.HashMap()) match {
    case rs: EsoRunState => rs
    case _ => state}
}

case class SaveBFLangsHandler(eio: EsoIOInterface = EsoConsoleInterface, efi: EsoFileInterface = SystemFileInterface) extends CommandHandler{
  val nam: String = "saveBFLangs"
  val helpStr: String = "Save user-defined BrainFuck translators ({-f :fileName:})"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val fnam = args.getOrElse("f", EsoDefaults.defBFLFile)
    val ignore = EsoDefaults.defTransVec.map(_.id)
    val jsoVec = state.trans.toVector.collect{
      case (id, bft: BFTranslator) if !ignore.contains(id) => (bft.name, bft.toJObject)}
    val namVec = jsoVec.map{case (k, _) => JString(k)}
    val jsObj = JObject.fromSeq(jsoVec :+ ("names", JArray.fromSeq(namVec)))
    efi.writeFile(fnam, jsObj.render())
    if(state.bools("log")) eio.println(s"Translators saved to $fnam.")
    
    state}
}

case class ShowSyntaxHandler(eio: EsoIOInterface = EsoConsoleInterface) extends CommandHandler{
  val nam: String = "syntax"
  val helpStr: String = "Show the syntax of a supported BrainFuck derivative ({-l :language:})"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val str = args.get("l") match{
      case Some(lang) => state.trans.toVector.collectFirst{
        case (_, t: BFTranslator) if t.name == lang => t} match{
        case Some(t) => s"Syntax for $lang...\n${t.kvPairs.map{case (k, v) => s"$k: $v"}.mkString("\n")}\n"
        case None => "Error: Language Not Recognized"}
      case _ => "Error: Not Enough Arguments"}
    eio.println(str)
    
    state}
}

object ClearBindingsHandler extends CommandHandler{
  val nam: String = "clrBindings"
  val helpStr: String = "Clear all current bindings"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = state.clearBinds
}

object ClearCacheHandler extends CommandHandler{
  val nam: String = "clrCache"
  val helpStr: String = "Clear the current built program cache"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = state.clearCache
}

case class LoadBindingsHandler(eio: EsoIOInterface = EsoConsoleInterface, efi: EsoFileInterface = SystemFileInterface) extends CommandHandler with LoadHandler{
  val nam: String = "loadBindings"
  val helpStr: String = "Load saved bindings ({-f :fileName:})"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val fnam = args.getOrElse("f", EsoDefaults.defBindFile)
    readStringMapJSON(efi, eio)(fnam) match{
      case Success(vec) => vec.foldLeft(state){case (s, (t, b)) => s.addBind(t, b)}}}
  
  def loadOnly(state: EsoRunState): EsoRunState = apply(state)(immutable.HashMap()) match {
    case rs: EsoRunState => rs
    case _ => state}
}

case class SaveBindingsHandler(efi: EsoFileInterface = SystemFileInterface) extends CommandHandler{
  val nam: String = "saveBindings"
  val helpStr: String = "Save user bindings ({-f :fileName:})"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val fnam = args.getOrElse("f", EsoDefaults.defBindFile)
    val bindVec = state.binds.toVector
    val nams = bindVec.map{case (k, _) => JString(k)}
    val jsoPairs = bindVec.map{case (k, v) => (k, JString(v))} :+ ("names", JArray.fromSeq(nams))
    val bindStr = JObject.fromSeq(jsoPairs).render()
    efi.writeFile(fnam, bindStr)
    
    state}
}

case class ListBindingsHandler(eio: EsoIOInterface = EsoConsoleInterface) extends CommandHandler{
  val nam: String = "listBindings"
  val helpStr: String = "List all current bindings"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val bindStr = state.binds.toVector
      .map{case (k, v) => s"- $k => $v"}
      .sorted
      .mkString("\n")
    
    val str =
      s"""|Bindings...
          |$bindStr
          |""".stripMargin
    eio.println(str)
    
    state}
}

case class SetVarHandler(eio: EsoIOInterface = EsoConsoleInterface) extends CommandHandler{
  val nam: String = "set"
  val helpStr: String = "Set runtime parameters ({-:varName: :value:}*)"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = args.toVector.foldLeft(state){
    case (s, (k, v)) => s.setVar(k, v) match{
      case Success(ns) => ns
      case Failure(e) =>
        val str = e match{
          case EsoExcep(s) => s
          case _ => e.toString}
        eio.println(s"Error: $str")
        s}}
}

object SetDefaultsHandler extends CommandHandler{
  val nam: String = "defaults"
  val helpStr: String = "Restore runtime state to defaults"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = EsoRunState.default
}

case class ListLangsHandler(eio: EsoIOInterface = EsoConsoleInterface) extends CommandHandler{
  val nam: String = "listLangs"
  val helpStr: String = "List all currently available language components"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val str =
      f"""|Languages...
          |${state.interps.values.map(i => s"- $i").toVector.sorted.mkString("\n")}
          |
          |Translators...
          |${state.trans.values.map(t => s"- $t").toVector.sorted.mkString("\n")}
          |
          |Transpilers...
          |${state.gens.values.map(t => s"- $t").toVector.sorted.mkString("\n")}
          |
          |""".stripMargin
    eio.print(str)
    
    state}
}

case class ListVarsHandler(eio: EsoIOInterface = EsoConsoleInterface) extends CommandHandler{
  val nam: String = "listVars"
  val helpStr: String = "List all runtime parameters with their current values"
  
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
    eio.println(str)
    
    state}
}

case class ListFileAssociationsHandler(eio: EsoIOInterface = EsoConsoleInterface) extends CommandHandler{
  val nam: String = "listFileAssociations"
  val helpStr: String = "List all currently recognized file associations"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val str =
      s"""|File Associations...
          |${state.fileAssoc.toVector.map{case (f, l) => s"- .$f => $l"}.sorted.mkString("\n")}
          |""".stripMargin
    eio.println(str)
    state}
}

case class SaveFileAssociationsHandler(efi: EsoFileInterface = SystemFileInterface) extends CommandHandler{
  val nam: String = "saveFileAssociations"
  val helpStr: String = "Save all currently recognized file associations ({-f :fileName:})"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val fnam = args.getOrElse("f", EsoDefaults.defAssocFile)
    val assVec = state.fileAssoc.toVector
    val exts = assVec.map{case (k, _) => JString(k)}
    val jsoPairs = assVec.map{case (k, v) => (k, JString(v))} :+ ("names", JArray.fromSeq(exts))
    val assocStr = JObject.fromSeq(jsoPairs).render()
    efi.writeFile(fnam, assocStr)
    state}
}

case class LoadFileAssociationsHandler(eio: EsoIOInterface = EsoConsoleInterface, efi: EsoFileInterface = SystemFileInterface) extends CommandHandler with LoadHandler{
  val nam: String = "loadFileAssociations"
  val helpStr: String = "Load file associations ({-f :fileName:})"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val fnam = args.getOrElse("f", EsoDefaults.defAssocFile)
    readStringMapJSON(efi, eio)(fnam) match{
      case Success(vec) => vec.foldLeft(state){case (s, (e, l)) => s.addAssoc(e, l)}}}
  
  def loadOnly(state: EsoRunState): EsoRunState = apply(state)(immutable.HashMap()) match {
    case rs: EsoRunState => rs
    case _ => state}
}

object AddFileAssociationHandler extends CommandHandler{
  val nam: String = "addFileAssociation"
  val helpStr: String = "Add a new file association ({-:ext: :lang:}*)"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    args.toVector.foldLeft(state){
      case (s, (e, l)) => s.addAssoc(e, l)}}
}

object DropFileAssociationHandler extends CommandHandler{
  val nam: String = "dropFileAssociation"
  val helpStr: String = "Forget a particular file association (<-e :ext:>)"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    args.get("e").map(state.dropAssoc).getOrElse(state)}
}

object ClearFileAssociationsHandler extends CommandHandler{
  val nam: String = "clearFileAssociations"
  val helpStr: String = "Forget all currently recognized file associations"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = state.clearAssoc
}

object ExitHandler extends CommandHandler{
  val nam: String = "exit"
  val helpStr: String = "Shut down Eso"
  
  def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = EsoHalt
}