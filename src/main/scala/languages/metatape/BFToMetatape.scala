package languages.metatape

import common.{Config, Transpiler}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.{Success, Try}

object BFToMetatape extends Transpiler{
  val src: String = "BrainFuck"
  val dst: String = "Metatape"
  
  private val opMap = immutable.HashMap[Char, String](
    '>' -> ">",
    '<' -> "<",
    '+' -> "!+",
    '-' -> "!-",
    '[' -> "!cee>(<xx[",
    ']' -> "!cee>(<xx]|<xx)|<xx)",
    ',' -> "!,",
    '.' -> "!.",
    '_' -> "n")
  
  def apply(config: Config)(progRaw: String): Try[String] = {
    val wid: Int = config.num("charWidth")
    val prog = filterChars(progRaw, "[]<>+-,.")
      .replaceAll("""\[[+\-]\]""", "_")
      .toVector
    
    def strFill(n: Int)(str: String): String = Vector.fill(n)(str).mkString
    @tailrec
    def cnt(c: Char, i: Int, n: Int = 0): (Int, Int) = prog.lift(i) match{
      case Some(`c`) => cnt(c, i + 1, n + 1)
      case _ => (n, i)}
    
    @tailrec
    def tdo(i: Int = 0, ac: Vector[String] = Vector()): String = prog.lift(i) match{
      case Some(c) => c match{
        case '+' => cnt(c, i) match{
          case (num, ni) =>
            val strs = num
              .toBinaryString
              .reverse
              .toVector
              .zipWithIndex
              .collect{case ('1', i) => i + 1}
              .map(i => s"${strFill(i)(">")}!+")
              .mkString
            tdo(ni, ac :+ s"eeexx${strs}x")}
        case '-' => cnt(c, i) match{
          case (num, ni) =>
            val strs = num
              .toBinaryString
              .reverse
              .toVector
              .zipWithIndex
              .collect{case ('1', i) => i}
              .map(i => s"!m${strFill(i)(">")}!-")
              .mkString
            tdo(ni, ac :+ s"eeexx${strs}x")}
        case _ => tdo(i + 1, ac :+ opMap(c))}
      case None => ac.mkString}
    
    val mtProg =
      s"""|@r{[[(e(|x<])|<])x}
          |@+{[(n>]|ex)!r}
          |@m{${strFill(wid + 1)(">")}ex${strFill(wid)("<")}}
          |@-{[(n|ex>])!r}
          |@,{eeexx${strFill(wid)(">")}${strFill(wid)("exi<")}x}
          |@.{e${strFill(wid)(">")}${strFill(wid)("o<")}x}
          |@f{!re>ex<x}
          |@c{eeex>n<x ${strFill(wid)(">(!f|")}!r${strFill(wid)(")")}x}
          |
          |${tdo()}""".stripMargin
    Success(mtProg)}
}
