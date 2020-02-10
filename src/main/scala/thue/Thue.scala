package thue

import common.{Config, EsoExcep, Interpreter}
import parsers.{EsoParser, RegexParser}

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object Thue extends Interpreter{
  val name: String = "Thue"
  
  val initReg: Regex = raw"""(?s)(.*)\n\s*::=\s*\n(.*)\z""".r
  val ruleParser: EsoParser[String, (String, String)] = RegexParser(raw"""(?m)^(.*)::=(.*)$$""".r){ m => (m.group(1), m.group(2))}
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = parse(progRaw) map{case (init, prog) => thi(init, prog, config.rands)}
  
  def thi(init: String, prog: Vector[(String, String)], initRands: LazyList[Int]): Seq[Char] => LazyList[Char] = {
    def collapse(inp: Seq[Char]): Seq[String] = LazyList.unfold(inp){lst =>
      if(lst.isEmpty) None
      else{
        val (hd, tl) = lst.span(_ != '\n')
        Some((hd.mkString, if(tl.startsWith("\n")) tl.tail else tl))}}
    
    @tailrec
    def tdo(ac: String, inp: Seq[String], rands: LazyList[Int]): Option[(String, (String, Seq[String], LazyList[Int]))] = rands match{
      case r +: rs =>
        val hits = prog.filter(p => ac.contains(p._1))
        if(hits.isEmpty) None
        else{
          val (k, v) = hits((r%hits.length + hits.length)%hits.length)
          v match{
            case ":::" => tdo(ac.replaceAllLiterally(k, inp.head), inp.tail, rs)
            case _ =>
              if(v.startsWith("~")) Some((v.tail, (ac.replaceAllLiterally(k, ""), inp, rs)))
              else tdo(ac.replaceAllLiterally(k, v), inp, rs)}}}
    inputs => LazyList.unfold((init, collapse(inputs), initRands)){case (ac, inp, rs) => tdo(ac, inp, rs)}.flatten}
  
  def parse(progRaw: String): Try[(String, Vector[(String, String)])] = {
    progRaw match{
      case initReg(rls, init) => Success((init, ruleParser.parseAllValues(rls)))
      case _ => Failure(EsoExcep("Malformed Program"))}}
}
