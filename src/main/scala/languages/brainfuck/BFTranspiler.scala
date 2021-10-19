package languages.brainfuck

import common.{Config, Transpiler}

import scala.util.Try
import scala.util.matching.Regex

trait BFTranspiler extends Transpiler{
  val src: String = "BrainFuck"
  val fSig: Regex = raw"""def (.*)\(\).*""".r
  
  def genProg(init: Int, olen: Int, dyn: Boolean, methSize: Int, prog: Vector[BFOp]): String
  
  def apply(config: Config)(progRaw: String): Try[String] = Try{(config.num("init"), config.num("olen"), config.num("methSize"), config.bool("dyn"), config.bool("indent"))} flatMap{
    case (init, olen, methSize, dyn, ind) =>
      BFOptimize(progRaw, comp=true) map{prog =>
        val gen = genProg(init, olen, dyn, methSize, prog)
        if(ind) indent(gen)
        else gen}}
  
  def incStr(n: Int): String = s"${if(n < 0) "-" else "+"}= ${n.abs}"
  def shiftStr(n: Int): String = s"${if (n < 0) "-" else "+"} ${n.abs}"
  def opStr(bop: SingOp): String = {
    val opstr = bop.ops map{
      case (i, Some(n)) => i match{
        case 0 => s"tape(p) ${incStr(n)}"
        case _ => s"tape(p ${shiftStr(i)}) ${incStr(n)}"}
      case (i, None) => s"tape(p ${shiftStr(i)}) = 0"}
    s"${opstr.mkString("\n")}${if(bop.shift != 0) s"\np ${incStr(bop.shift)}" else ""}"}
  def lopStr(bop: LoopOp): String = {
    val opstr = bop.ops
      .filter(_._1 != 0)
      .map {
        case (i, arg) => arg match{
          case Some(n) => s"tape(p ${shiftStr(i)}) ${incStr(n)}*tmp"
          case None => s"tape(p ${shiftStr(i)}) = 0"}}
    s"""|if(tape(p) != 0){
        |val tmp = tape(p)
        |${opstr.mkString("\n")}
        |tape(p) = 0
        |}""".stripMargin}
  
  def segScala(block: Vector[String], methSize: Int, olen: Int = -1): Vector[String] = {
    if(block.sizeIs < methSize - 1) Vector(block.mkString("\n"))
    else{
      val fnam = block.head match{
        case fSig(str) => str}
      val groups = block
        .tail.init
        .grouped(methSize)
        .to(LazyList)
      val calls = groups
        .indices
        .map{i => s"${fnam}s$i()${if(olen >= 0) "\nif(end) return ()" else ""}"}
        .toVector
      val funcs = groups
        .zipWithIndex
        .map{
          case (vec, i) =>
            s"""|def ${fnam}s$i(): Unit = {
                |${vec.mkString("\n")}
                |}""".stripMargin}
      
      segScala(block.head +: (calls :+ "}"), methSize, olen) ++ funcs.toVector}}
}
