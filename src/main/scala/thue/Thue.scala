package thue

import common.{Config, Interpreter}
import parsers.{EsoParser, RegexParser}

import scala.annotation.tailrec
import scala.util.Try

object Thue extends Interpreter{
  val name: String = "Thue"
  
  val thueParser: EsoParser[String, (Vector[(String, String)], String)] = {
    val spcReg = """\s*""".r
    val pairParser = RegexParser("""(?m)^(.*)::=(.*)$$""")(m => (m.group(1), m.group(2)))
    val ruleParser = pairParser.onlyIf(p => !spcReg.matches(p._1 ++ p._2))
    ruleParser.* <&> pairParser.after}
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = thueParser(progRaw).toTry() map{case (prog, init) => thi(init, prog, config.rands)}
  
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
}
