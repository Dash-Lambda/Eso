package prelude

import common.{Config, Transpiler}

import scala.annotation.tailrec
import scala.util.Try
import scala.util.matching.Regex

object BFToPrelude extends Transpiler{
  val src: String = "BrainFuck"
  val dst: String = "Prelude"
  
  val incReg: Regex = raw"""(\+{1,9})(.*)\z""".r
  val decReg: Regex = raw"""(-{1,9})(.*)\z""".r
  val opReg: Regex = raw"""(.)(.*)\z""".r
  
  def apply(config: Config)(progRaw: String): Try[String] = {
    @tailrec
    def tdo(v1: Vector[Char], v2: Vector[Char], src: String): String = src match{
      case incReg(s, ops) => tdo(v1 :+ s.length.toString.head :+ '+', v2 :+ ' ' :+ ' ', ops)
      case decReg(s, ops) => tdo(v1 :+ s.length.toString.head :+ '-', v2 :+ ' ' :+ ' ', ops)
      case opReg(op, ops) =>
        val (p1, p2) = op match{
          case ">" => ("v", "#")
          case "<" => ("#", "^")
          case "[" => ("(", " ")
          case "]" => (")", " ")
          case "," => ("?", " ")
          case "." => ("!v", "^#")}
        tdo(v1 :++ p1, v2 :++ p2, ops)
      case _ =>
        s"""|${v1.mkString}
            |${v2.mkString}""".stripMargin}
    Try{tdo(Vector(), Vector(), filterChars(progRaw, "[]<>+-,."))}}
}
