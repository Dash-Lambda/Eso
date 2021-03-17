package thue

import common.{Config, Interpreter}
import parsers.EsoParser
import parsers.Implicits._

import scala.annotation.tailrec
import scala.util.Try

object Thue extends Interpreter{
  val name: String = "Thue"
  
  def ruleParse: EsoParser[(String, String)] = """(?m)^(.*\S.*)::=""".r <&> """^(.*)\v""".r
  def initParse: EsoParser[String] = """(?m)^\s*::=.*\v""".r &> "(?s).*".r
  def thueParse: EsoParser[(Vector[(String, String)], String)] = ruleParse.* <&> initParse
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = thueParse(progRaw).toTry() map{case (prog, init) => thi(init, prog, config.rands)}
  
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
            case ":::" => tdo(ac.replace(k, inp.head), inp.tail, rs)
            case _ =>
              if(v.startsWith("~")) Some((v.tail, (ac.replace(k, ""), inp, rs)))
              else tdo(ac.replace(k, v), inp, rs)}}}
    inputs => LazyList.unfold((init, collapse(inputs), initRands)){case (ac, inp, rs) => tdo(ac, inp, rs)}.flatten}
}
