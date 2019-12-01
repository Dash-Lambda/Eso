package thue

import common.{Config, EsoExcep, Interpreter}

import scala.annotation.tailrec
import scala.util.{Failure, Random, Success, Try}

object Thue extends Interpreter{
  val name: String = "Thue"
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = condition(progRaw) map{case (init, prog) => thi(init, prog)}
  
  def thi(init: String, prog: Vector[(String, String)]): Seq[Char] => LazyList[Char] = {
    val rand = new Random()
    
    def collapse(inp: Seq[Char]): Seq[String] = LazyList.unfold(inp){lst =>
      if(lst.isEmpty) None
      else{
        val (hd, tl) = lst.span(_ != '\n')
        Some((hd.mkString, if(tl.startsWith("\n")) tl.tail else tl))
      }
    }
    
    @tailrec
    def nxt(ac: String, inp: Seq[String]): Option[(String, (String, Seq[String]))] = {
      val hit = {
        if(prog.exists(p => ac.contains(p._1))){
          val hits = prog.filter(p => ac.contains(p._1))
          Some(hits(rand.nextInt(hits.length)))
        }else None
      }
      
      hit match{
        case Some((k, v)) => v match{
          case ":::" => nxt(ac.replaceAllLiterally(k, inp.head), inp.tail)
          case _ =>
            if(v.startsWith("~")) Some((v.tail, (ac.replaceAllLiterally(k, ""), inp)))
            else nxt(ac.replaceAllLiterally(k, v), inp)
        }
        case None => None
      }
    }
    
    inputs => LazyList.unfold((init, collapse(inputs))){case (ac, inp) => nxt(ac, inp)}.flatten
  }
  
  def condition(progRaw: String): Try[(String, Vector[(String, String)])] = {
    val ruleExp = raw"""\A(.*)::=(.*)\z""".r
    
    val lines = progRaw
      .linesIterator
      .to(LazyList)
      .filter(_.nonEmpty)
    val prog = lines
      .takeWhile(str => !str.startsWith("::="))
      .collect{case ruleExp(k, v) => (k, v)}
    val conditioned = lines
      .dropWhile(str => !str.startsWith("::="))
      .collectFirst{case str if !str.contains("::=") => (str, prog.toVector)}
    
    conditioned match{
      case Some(p) => Success(p)
      case None => Failure(EsoExcep("Malformed Program"))
    }
  }
}
