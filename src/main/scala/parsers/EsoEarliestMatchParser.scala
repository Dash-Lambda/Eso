package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

case class EsoEarliestMatchParser[+A](ps: Vector[EsoParser[A]]) extends EsoParser[A] {
  def apply(inp: String): EsoParseRes[A] = tramp(inp)(done).result
  
  override def ||[B >: A](q: => EsoParser[B]): EsoParser[B] = q match {
    case EsoEarliestMatchParser(qs) => EsoEarliestMatchParser(ps ++ qs)
    case _ => EsoEarliestMatchParser(ps :+ q)
  }
  
  override def tramp[B](inp: String)(cc: EsoParseRes[A] => TailRec[EsoParseRes[B]]): TailRec[EsoParseRes[B]] = {
    def rec(pv: Vector[EsoParser[A]], firstMatch: EsoParseRes[A])(res: EsoParseRes[A]): TailRec[EsoParseRes[B]] = {
      val nxt = {
        if (res.passed && (res.start < firstMatch.start || !firstMatch.passed)) res
        else firstMatch
      }
      pv match {
        case p +: pp => tailcall(p.tramp(inp)(rec(pp, nxt)))
        case _ => cc(firstMatch)
      }
    }
    
    tailcall(rec(ps, EsoParseFail)(EsoParseFail))
  }
}
