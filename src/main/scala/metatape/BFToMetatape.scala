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
    val prog = progRaw.toVector.filter("[]<>+-,.".contains(_))
    val opMap = immutable.HashMap[Char, String](
      '>' -> ">",
      '<' -> "<",
      '+' -> "!+",
      '-' -> "!-",
      '[' -> "!cee>(<xx[",
      ']' -> "!cee>(<xx]|<xx)|<xx)",
      ',' -> "!,",
      '.' -> "!.")
    
    @tailrec
    def tdo(src: Vector[Char], ac: String = ""): String = src match{
      case c +: cs => tdo(cs, ac ++ opMap(c))
      case _ => ac}
    
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
          |${tdo(prog)}""".stripMargin
    Success(mtProg)}
}
