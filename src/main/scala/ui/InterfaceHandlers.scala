package ui

import java.io.{File, PrintWriter}

import brainfuck.{BFTranslator, GenBFT}
import common.{EsoExcep, EsoObj, MapExtractor}

import scala.annotation.tailrec
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
  
  def findTransPath(state: EsoRunState, sl: String, tls: Seq[String]): Option[(String, String => Try[String])] = tls.iterator.map(tl => (tl, buildTrans(state)(sl, tl))).collectFirst{case (nam, Some(t)) => (nam, t)}
  def findTransPath(state: EsoRunState, sls: Seq[String], tl: String): Option[(String, String => Try[String])] = sls.iterator.map(sl => (sl, buildTrans(state)(sl, tl))).collectFirst{case (nam, Some(t)) => (nam, t)}
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
            case _ => Vector[String]()}
          nxt.filter(b => !c.contains(b)).map(l => c :+ l)}}
      .takeWhile(_.nonEmpty)
      .flatten
      .collectFirst{case vec if vec.last == tl => compChain(vec)}}
  
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
    val res = src.mkString
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
  
  def timeIt[T](thing: => T): (T, Long) = {
    val t = System.currentTimeMillis
    val res = thing
    (res, System.currentTimeMillis - t)}
}

object DebugHandler extends InterfaceHandler{
  override val nam: String = "debug"
  override val helpStr: String = "Unsafe run handler, for getting detailed error information out of the runtime while writing new components"
  
  val fextReg: Regex = raw""".*\.(\w+)\z""".r
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    def inputs: Try[LazyList[Char]] = args.get("i") match{
      case Some(fnam) => readFile(fnam) map (_.to(LazyList))
      case None => Success(LazyList.continually(StdIn.readLine + '\n').flatten)}
    
    doOrOp(args.get("s"), "Missing Source File"){src =>
      val langOp = args.get("l") match{
        case None => src match{
          case fextReg(fext) => EsoDefaults.fileExtensionMap.get(fext)
          case _ => None}
        case l => l}
      doOrOp(langOp, "Unrecognized Language or File Extension"){lang =>
        doOrOp(findTransPath(state, lang, state.interpNames), "Language Not Recognized"){
          case (inam, t) =>
            doOrErr(readFile(src)){progRaw =>
              print("Building Interpreter... ")
              val prog = t(progRaw) match{case Success(str) => str}
              val i = state.interps(inam)(state.config)(prog)
              println("Done.")
              doOrErr(i){r =>
                doOrErr(inputs){inp =>
                  val res = r(inp)
                  res.foreach(print)
                  println("\nProgram Completed")}}}}}}
    state}
}

object RunProgHandler extends InterfaceHandler{
  val nam: String = "run"
  override val helpStr: String = "<-s :sourceFileName:> {-l :language:, -i :inputFileName:, -o :outputFileName:}"
  
  val fextReg: Regex = raw""".*\.(\w+)\z""".r
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val printNum = state.bools("printNum")
    def olim(res: LazyList[Char]): LazyList[Char] = state.nums("olen") match{
      case -1 => res
      case n => res.take(n)}
    
    def inputs: Try[LazyList[Char]] = args.get("i") match{
      case Some(fnam) => readFile(fnam) map (_.to(LazyList))
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
      val langOp = args.get("l") match{
        case None => src match{
          case fextReg(fext) => EsoDefaults.fileExtensionMap.get(fext)
          case _ => None}
        case l => l}
      doOrOp(langOp, "Unrecognized Language or File Extension"){lang =>
        doOrOp(findTransPath(state, lang, state.interpNames), "Language Not Recognized"){
          case (inam, t) =>
            doOrErr(readFile(src)){progRaw =>
              print("Building Interpreter... ")
              doOrErr(t(progRaw)){prog =>
                val (i, bdr) = timeIt(state.interps(inam)(state.config)(prog))
                println(s"Done in ${bdr}ms.")
                doOrErr(i){r =>
                  doOrErr(inputs){inp =>
                    val (flg, rdr) = timeIt{
                      val res = r(inp)
                      tryAll{printer(olim(res))}}
                    flg match{
                      case Success(_) => println(s"\nProgram completed in ${rdr}ms")
                      case Failure(e) => println(s"\nError: $e\nProgram failed in ${rdr}ms")}}}}}}}}
    
    state}
}

object TranslateHandler extends InterfaceHandler{
  override val nam: String = "translate"
  override val helpStr: String = "<-sl :sourceLanguage, -tl :targetLanguage, -s :sourceFileName:> {-o :targetFileName:}"
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val ext = MapExtractor(args)
    
    def printer(str: String): Unit = args.get("o") match{
      case Some(onam) =>
        writeFile(onam, str)
        println(s"Translation saved to $onam.")
      case None => println(str)}
    
    Vector("sl", "tl", "s") match{
      case ext(sl, tl, i) =>
        doOrOp(buildTrans(state)(sl, tl), "No Applicable Translation Path"){t =>
          doOrErr(readFile(i)){progRaw =>
            doOrErr(t(progRaw)){prog =>
              printer(prog)}}}
      case _ => println("Error: Not Enough Arguments")}
    
    state}
}

object TranspileHandler extends InterfaceHandler{
  override val nam: String = "transpile"
  override val helpStr: String = "<-sl :sourceLanguage, -tl :targetLanguage, -s :sourceFileName:> {-o :targetFileName:}"
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val ext = MapExtractor(args)
    
    def printer(str: String): Unit = args.get("o") match{
      case Some(onam) =>
        writeFile(onam, str)
        println(s"Transpiled program saved to $onam.")
      case None => println(str)}
    
    Vector("sl", "tl", "s") match{
      case ext(sl, tl, s) =>
        val ins = state.genNames.map(_._1)
        val links = state.genLinks
        doOrOp(findTransPath(state, sl, ins), "No Applicable Translation Path"){
          case (lin, tin) =>
            doOrOp(findTransPath(state, links(lin), tl), "No Applicable Translation Path"){
              case (lout, tout) =>
                doOrErr(readFile(s)){progRaw =>
                  doOrErr(tin(progRaw)){prog1 =>
                    doOrErr(state.gens((lin, lout))(state.config)(prog1)){prog2 =>
                      doOrErr(tout(prog2)){prog3 =>
                        printer(prog3)}}}}}}
      case _ => println("Error: Not Enough Arguments")}
    
    state}
}

object DefineBFLangHandler extends InterfaceHandler{
  override val nam: String = "defineBFLang"
  override val helpStr: String = ""
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val nam = StdIn.readLine("Name: ")
    val syn = Vector("[", "]", "<", ">", "+", "-", ",", ".") map{c => (c, StdIn.readLine(s"$c => "))}
    state.addTrans(GenBFT(nam, syn))}
}

object LoadBFLangsHandler extends InterfaceHandler{
  override val nam: String = "loadBFLangs"
  override val helpStr: String = "{-f :fileName:}"
  
  private val bfNamReg = raw"""(?s)[^\#]*\#(\V*)(.*)\z""".r
  private val bfBindReg = raw"""(?s)[^\[\]\+\-\<\>\,\.]*([\[\]\+\-\<\>\,\.])\=\>(\V*)\z""".r
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    @tailrec
    def cdo(src: Vector[Char], ac: Vector[(String, String)] = Vector()): (Vector[(String, String)], String) = src match{
      case c +: '=' +: '>' +: cs if "[]<>+-,.".contains(c) && ac.sizeIs < 8 =>
        val bind = cs.takeWhile(_ != '\n')
        val tl = cs.dropWhile(_ != '\n').drop(1)
        cdo(tl, ac :+ (c.toString, bind.mkString))
      case _ +: cs if ac.sizeIs < 8 => cdo(cs, ac)
      case _ => (ac, src.mkString)}
    
    @tailrec
    def mkTrans(str: String, ac: Vector[BFTranslator] = Vector()): Vector[BFTranslator] = str match{
      case bfNamReg(nam, bod) =>
        cdo(bod.toVector) match{
          case (vec, tl) if "[]<>+-,.".forall(c => vec.map(_._1).contains(c.toString)) => mkTrans(tl, ac :+ GenBFT(nam, vec))
          case _ => mkTrans(bod, ac)}
      case _ => ac}
    
    val fnam = args.get("f") match{
      case Some(str) => str
      case None => EsoDefaults.defBFLFile}
    
    doOrErr(readFile(fnam))(mkTrans(_)) match{
      case Some(bfts) =>
        println(s"Loaded BF Langs:\n${bfts.map(t => s"- ${t.name}").mkString("\n")}")
        state.addAllTrans(bfts)
      case None => state}}
}

object SaveBFLangsHandler extends InterfaceHandler{
  override val nam: String = "saveBFLangs"
  override val helpStr: String = "{-f :fileName:}"
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val fnam = args.get("f") match{
      case Some(str) => str
      case None => EsoDefaults.defBFLFile}
    val ignore = EsoDefaults.defTransVec.map(_.id)
    val bfStr = state.trans.collect{
      case (id, bft: BFTranslator) if !ignore.contains(id) =>
        s"""|#${bft.name}
            |${bft.syntax.map{case (k, v) => s"$k=>$v"}.mkString("\n")}""".stripMargin}
      .mkString("\n")
    
    writeFile(fnam, bfStr)
    
    state}
}

object ShowSyntaxHandler extends InterfaceHandler{
  override val nam: String = "syntax"
  override val helpStr: String = "{-l :language:}"
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
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
  override val nam: String = "clrBindings"
  override val helpStr: String = ""
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = state.clearBinds
}

object LoadBindingsHandler extends InterfaceHandler{
  override val nam: String = "loadBindings"
  override val helpStr: String = "{-f :fileName:}"
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val breg = raw"""(\S+) (.*)\z""".r
    val fnam = args.get("f") match{
      case Some(str) => str
      case None => EsoDefaults.defBindFile}
    
    doOrErr(readFile(fnam)){str =>
      str
        .linesIterator
        .collect{
          case breg(t, b) => (t, b)}
        .foldLeft(state){
          case (s, (t, b)) => s.addBind(t, b)}} match{
      case Some(s) => s
      case None => state}}
}

object SaveBindingsHandler extends InterfaceHandler{
  override val nam: String = "saveBindings"
  override val helpStr: String = "{-f :fileName:}"
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val fnam = args.get("f") match{
      case Some(str) => str
      case None => EsoDefaults.defBindFile}
    
    val bindStr = state.binds.toVector.map{case (k, v) => s"$k $v"}.mkString("\n")
    writeFile(fnam, bindStr)
    
    state}
}

object ListBindingsHandler extends InterfaceHandler{
  override val nam: String = "listBindings"
  override val helpStr: String = ""
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
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
  override val nam: String = "set"
  override val helpStr: String = "{-:varName: :value:}*"
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = args.toVector.foldLeft(state){case (s, (k, v)) => s.setVar(k, v)}
}

object SetDefaultsHandler extends InterfaceHandler{
  override val nam: String = "defaults"
  override val helpStr: String = ""
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = EsoRunState.default
}

object ListLangsHandler extends InterfaceHandler{
  override val nam: String = "listLangs"
  override val helpStr: String = ""
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
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
  override val nam: String = "listVars"
  override val helpStr: String = ""
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
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
  override val nam: String = "listFileAssociations"
  override val helpStr: String = ""
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = {
    val fStr = EsoDefaults.fileExtensionsVec.map{case (f, l) => s"- .$f => $l"}.mkString("\n")
    val str =
      s"""|File Associations...
          |$fStr
          |""".stripMargin
    println(str)
    state}
}

object ExitHandler extends InterfaceHandler{
  override val nam: String = "exit"
  override val helpStr: String = ""
  
  override def apply(state: EsoRunState)(args: HashMap[String, String]): EsoState = EsoHalt
}