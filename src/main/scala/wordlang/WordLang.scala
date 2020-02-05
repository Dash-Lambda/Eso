package wordlang

import common.{Config, Interpreter}

import scala.collection.immutable
import scala.util.Try

object WordLang extends Interpreter{
  val name: String = "WordLang"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = ???
  
  def parse(progRaw: String): (Vector[WOP], immutable.HashMap[String, Int]) = {
    val escapeReg = raw"""\\(.)""".r
    val escaped = escapeReg.replaceAllIn(progRaw, m => (-m.group(1).head).toChar.toString)
    ???
  }
  
  trait WOP
  case class CharOp(c: Char) extends WOP
  object IncToggle extends WOP
  object PrintChar extends WOP
  case class IncByVar(name: String) extends WOP
  case class IncVarBy(name: String) extends WOP
  object ReadChar extends WOP
  case class Jump(name: String) extends WOP
  case class JumpLess(name: String) extends WOP
  case class JumpGreater(name: String) extends WOP
  case class PrintNum(code: Vector[WOP]) extends WOP
}
