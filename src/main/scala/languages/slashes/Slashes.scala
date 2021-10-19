package languages.slashes

import common.{Config, Interpreter}

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.{Success, Try}

object Slashes extends Interpreter{
  val name: String = "///"
  val patrepReg: Regex = raw"""(?s)(?m)\/((?:[^\/]|\\\/)*)\/((?:[^\/]|\\\/)*)\/(.*)\z""".r
  val partRepReg: Regex = raw"""(?s)(?m)/.*\z""".r
  val popReg: Regex = raw"""(?s)(?m)\\?(.)(.*)\z""".r
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = Success(_ => slashRun(progRaw))
  
  def slashRun(progRaw: String): LazyList[Char] = {
    @tailrec
    def nxt(src: String): Option[(Char, String)] = src match{
      case patrepReg(p, r, str) => nxt(Iterator.iterate(str)(_.replace(p, r)).find(s => !s.contains(p)).get)
      case partRepReg() => None
      case popReg(c, s) => Some((c.head, s))
      case _ => None}
    LazyList.unfold(progRaw.replace("]\n[", ""))(nxt)}
}
