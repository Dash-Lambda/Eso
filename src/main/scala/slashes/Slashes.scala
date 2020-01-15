package slashes

import common.{Config, Interpreter}

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.{Success, Try}

object Slashes extends Interpreter{
  val name: String = "///"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = Success(_ => slashRun(progRaw))
  
  def slashRun(progRaw: String): LazyList[Char] = {
    def rep(str: String): String = {
      val (p, r, tail) = {
        @tailrec
        def bite(src: Vector[Char], ac: String = ""): (String, Vector[Char]) = src match{
          case '\\' +: c +: cs => bite(cs, ac + c)
          case '/' +: cs => (ac, cs)
          case c +: cs => bite(cs, ac + c)
          case _ => (ac, Vector())}
        val (p, t1) = bite(str.toVector)
        val (r,  t2) = bite(t1)
        (p, r, t2.mkString)}
      
      @tailrec
      def rdo(src: String): String = {
        if(src.contains(p)) rdo(Regex.quote(p).r.replaceFirstIn(src, Regex.quoteReplacement(r)))
        else src}
      rdo(tail)}
    
    @tailrec
    def nxt(src: String): Option[(Char, String)] = src.headOption match{
      case Some(c) => c match{
        case '\\' => if(src.sizeIs > 1) Some((src(1), src.drop(2))) else None
        case '/' => nxt(rep(src.tail))
        case _ => Some((c, src.tail))}
      case None => None}
    LazyList.unfold(progRaw.replaceAllLiterally("]\n[", ""))(nxt)}
}
