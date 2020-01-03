package metatape

import common.{Config, Transpiler}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.{Success, Try}

object BFToMetatape extends Transpiler{
  val src: String = "BrainFuck"
  val dst: String = "Metatape"
  
  def apply(config: Config)(progRaw: String): Try[String] = {
    val wid: Int = config.num("mtCharWidth")
    val prog = progRaw.filter("[]<>+-,.".contains(_)).replaceAll("""\[[+\-]\]""", "_").toVector
    val opMap = immutable.HashMap[Char, String](
      '>' -> ">",
      '<' -> "<",
      '+' -> "!+",
      '-' -> "!-",
      '[' -> "!cee>(<xx[",
      ']' -> "!cee>(<xx]|<xx)|<xx)",
      ',' -> "!,",
      '.' -> "!.",
      '_' -> "n")
    
    @tailrec
    def tdo(i: Int = 0, ac: String = ""): String = prog.lift(i) match{
      case Some(c) => tdo(i + 1, ac ++ opMap(c))
      case None => ac}
    
    def strFill(n: Int)(str: String): String = Vector.fill(n)(str).mkString
    
    val mtProg =
      s"""|@r{[[(e(|x<])|<])x}
          |@+{eeexx>[(n>]|ex)!rx}
          |@-{eeexx${strFill(wid)(">")}>ex${strFill(wid)("<")}[(n|ex>])!rx}
          |@,{eeexx${strFill(wid)(">")}${strFill(wid)("exi<")}x}
          |@.{e${strFill(wid)(">")}${strFill(wid)("o<")}x}
          |@f{!re>ex<x}
          |@c{eeex>n<x ${strFill(wid)(">(!f|")}!r${strFill(wid)(")")}x}
          |
          |${tdo()}""".stripMargin
    Success(mtProg)}
}
