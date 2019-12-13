package ui

import java.io.{File, PrintWriter}

import brainfuck.{BFTranslator, GenBFT}
import common.{EsoExcep, EsoObj, MapExtractor}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.io.{Source, StdIn}
import scala.util.{Failure, Success, Try}

object InterfaceHandlers extends EsoObj{
  val encodings: LazyList[String] = LazyList("UTF-8", "Cp1252", "UTF-16")
  val helpText: String =
    s"""|- run <-l :language:, -s :sourceFileName:> {-i :inputFileName:, -o :outputFileName:}
        |- transpile <-sl :sourceLanguage, -tl :targetLanguage, -s :sourceFileName:> {-o :targetFileName:}
        |- translate <-sl :sourceLanguage, -tl :targetLanguage, -s :sourceFileName:> {-o :targetFileName:}
        |
        |- defineBFLang
        |- loadBFLangs {-f :fileName:}
        |- saveBFLangs {-f :fileName:}
        |- syntax {-l :language:}
        |
        |- bind <command>
        |- unbind <token>
        |- clrBindings
        |- loadBindings {-f :fileName:}
        |- saveBindings {-f :fileName:}
        |- listBindings {-f :fileName:}
        |
        |- set {-:varName: :value:}*
        |- defaults
        |
        |- listLangs
        |- listVars
        |- help
        |
        |- exit
        |
        | Syntax:
        |- <expr>: Required
        |- {expr}: Optional
        |- expr*: any number of expressions
        |""".stripMargin
  
  def runProg(state: EsoRunState)(args: immutable.HashMap[String, String]): EsoState = {
    val ext = MapExtractor(args)
    
    def olim(res: LazyList[Char]): LazyList[Char] = state.nums("olen") match{
      case -1 => res
      case n => res.take(n)}
    
    def inputs: Try[LazyList[Char]] = args.get("i") match{
      case Some(fnam) => readFile(fnam) map (_.to(LazyList))
      case None => Success(LazyList.continually(StdIn.readLine).flatten)}
    
    def printer(out: Seq[Char]): Unit = args.get("o") match{
      case Some(onam) =>
        val of = new PrintWriter(new File(onam))
        out foreach{c =>
          print(c)
          of.print(c)
          of.flush()}
        of.close()
      case None => out foreach print}
    
    Vector("l", "s") match{
      case ext(lang, src) =>
        doOrOp(findTransPath(state, lang, state.interpNames), "Language Not Recognized"){
          case (inam, t) =>
            doOrErr(readFile(src)){progRaw =>
              print(s"Building Interpreter... ")
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
                      case Failure(e) => println(s"\nError: $e\nProgram failed in ${rdr}ms")}}}}}}
      case _ => println("Error: Not Enough Arguments")}
    
    state
  }
  
  def transProg(state: EsoRunState)(args: immutable.HashMap[String, String]): EsoState = {
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
    
    state
  }
  
  def genProg(state: EsoRunState)(args: immutable.HashMap[String, String]): EsoState = {
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
    
    state
  }
  
  def defineBFLang(state: EsoRunState): EsoState = {
    val nam = StdIn.readLine("Name: ")
    val syn = Vector("[", "]", "<", ">", "+", "-", ",", ".") map{c => (c, StdIn.readLine(s"$c => "))}
    state.addTrans(GenBFT(nam, syn))
  }
  
  def loadBFLangs(state: EsoRunState)(args: immutable.HashMap[String, String]): EsoState = {
    val bflReg = raw"""[^#]*#(\S+)\n((?:[\+-,\.<>\[\]]=>[^\n]+\n*){8})(.*)\z""".r
    val bfcReg = raw"""[^\+-,\.<>\[\]]*([\+-,\.<>\[\]])=>([^\n]+)(.*)\z""".r
    
    
    @tailrec
    def cdo(src: String, ac: Vector[(String, String)] = Vector()): Option[Vector[(String, String)]] = src match{
      case bfcReg(k, v, cs) => cdo(cs, ac :+ (k, v))
      case _ =>
        if("[]<>+-,.".forall(c => ac.exists(_._1 == c.toString))) Some(ac)
        else None}
    def mkTrans(str: String, ac: Vector[BFTranslator] = Vector()): Vector[BFTranslator] = str match{
      case bflReg(nam, cs, tl) => cdo(cs) match{
        case Some(ps) => mkTrans(tl, ac :+ GenBFT(nam, ps))
        case None => mkTrans(tl, ac)}
      case _ => ac}
    
    val fnam = args.get("f") match{
      case Some(str) => str
      case None => EsoDefaults.defBFLFile}
    doOrErr(readFile(fnam))(mkTrans(_)) match{
      case Some(bfts) => state.addAllTrans(bfts)
      case None => state}
  }
  
  def saveBFLangs(state: EsoRunState)(args: immutable.HashMap[String, String]): EsoState = {
    val fnam = args.get("f") match{
      case Some(str) => str
      case None => EsoDefaults.defBFLFile}
    val bfStr = state.trans.values
      .collect{
        case bft: BFTranslator =>
          s"""|#${bft.name}
              |${bft.syntax.map{case (k, v) => s"$k=>$v"}.mkString("\n")}""".stripMargin}
      .mkString("\n")
    writeFile(fnam, bfStr)
    
    state
  }
  
  def syntax(state: EsoRunState)(args: immutable.HashMap[String, String]): EsoState = {
    val str = args.get("l") match{
      case Some(lang) => state.trans.toVector.collectFirst{
        case (_, t: BFTranslator) if t.name == lang => t} match{
        case Some(t) => s"Syntax for $lang...\n${t.kvPairs.map{case (k, v) => s"$k: $v"}.mkString("\n")}\n"
        case None => "Error: Language Not Recognized"}
      case _ => "Error: Not Enough Arguments"}
    println(str)
    
    state
  }
  
  def clrBindings(state: EsoRunState): EsoState = state.clearBinds
  
  def loadBindings(state: EsoRunState)(args: immutable.HashMap[String, String]): EsoState = {
    val breg = raw"""(\S+) (.*)\z""".r
    val fnam = args.get("f") match{
      case Some(str) => str
      case None => EsoDefaults.defBindFile
    }
    doOrErr(readFile(fnam)){str =>
      str
        .linesIterator
        .collect{
          case breg(t, b) => (t, b)}
        .foldLeft(state){
          case (s, (t, b)) => s.addBind(t, b)}} match{
      case Some(s) => s
      case None => state}
  }
  
  def saveBindings(state: EsoRunState)(args: immutable.HashMap[String, String]): EsoState = {
    val fnam = args.get("f") match{
      case Some(str) => str
      case None => EsoDefaults.defBindFile}
    
    val bindStr = state.binds.toVector.map{case (k, v) => s"$k $v"}.mkString("\n")
    writeFile(fnam, bindStr)
    
    state
  }
  
  def listBindings(state: EsoRunState): EsoState = {
    val bindStr = state.binds.toVector
      .map{case (k, v) => s"- $k => $v"}
      .sorted
      .mkString("\n")
    
    val str =
      s"""|Bindings...
          |$bindStr
          |""".stripMargin
    println(str)
    
    state
  }
  
  def listLangs(state: EsoRunState): EsoState = {
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
    
    state
  }
  
  def setEnvVars(state: EsoRunState)(args: immutable.HashMap[String, String]): EsoState = args.toVector.foldLeft(state){case (s, (k, v)) => s.setVar(k, v)}
  
  def listEnvVars(state: EsoRunState): EsoState = {
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
    
    state
  }
  
  def showHelp(state: EsoRunState): EsoState = {
    println(helpText)
    state
  }
  
  def exit(): EsoState = {
    println("Shutting down...")
    EsoHalt
  }
  
  def unknown(state: EsoRunState): EsoState = {
    println("Error: Invalid Command")
    state
  }
  
  def findTransPath(state: EsoRunState, sl: String, tls: Seq[String]): Option[(String, String => Try[String])] = {
    tls.iterator.map(tl => (tl, buildTrans(state)(sl, tl))).collectFirst{case (nam, Some(t)) => (nam, t)}
  }
  def findTransPath(state: EsoRunState, sls: Seq[String], tl: String): Option[(String, String => Try[String])] = {
    sls.iterator.map(sl => (sl, buildTrans(state)(sl, tl))).collectFirst{case (nam, Some(t)) => (nam, t)}
  }
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
      .collectFirst{case vec if vec.last == tl => compChain(vec)}
  }
  
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
    res
  }
  def writeFile(fnam: String, str: String): Unit = {
    val oFile = new PrintWriter(new File(fnam))
    oFile.print(str)
    oFile.close()
  }
  
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
    (res, System.currentTimeMillis - t)
  }
}