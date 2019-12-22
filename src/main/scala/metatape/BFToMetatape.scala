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
      '+' -> "!{inc}",
      '-' -> "!{dec}",
      '[' -> "!{chk}ee>(<xx[",
      ']' -> "!{chk}ee>(<xx]|<xx)|<xx)",
      ',' -> "!{read}",
      '.' -> "!{print}")
    
    @tailrec
    def tdo(src: Vector[Char], ac: String = ""): String = src match{
      case c +: cs => tdo(cs, ac ++ opMap(c))
      case _ => ac}
    
    def strFill(n: Int)(str: String): String = Vector.fill(n)(str).mkString
    
    val mtProg =
      s"""|@reset{[[(e(|x<])|<])x}
          |@inc{eeexx>[(n>]|ex)!{reset}x}
          |@dec{eeexx${strFill(wid)(">")}>ex${strFill(wid)("<")}[(n|ex>])!{reset}x}
          |@read{eeexx${strFill(wid)(">")}${strFill(wid)("exi<")}x}
          |@print{e${strFill(wid)(">")}${strFill(wid)("o<")}x}
          |@fail{!{reset}e>ex<x}
          |@chk{eeex>n<x ${strFill(wid)(">(!{fail}|")}!{reset}${strFill(wid)(")")}x}
          |
          |${tdo(prog)}""".stripMargin
    Success(mtProg)}
}
