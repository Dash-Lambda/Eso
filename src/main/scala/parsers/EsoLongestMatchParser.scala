package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

case class EsoLongestMatchParser[+A](ps: Vector[EsoParser[A]]) extends EsoParser[A] {
  def apply(inp: String): EsoParseRes[A] = tramp(inp)(done).result
  
  override def |||[B >: A](q: => EsoParser[B]): EsoParser[B] = q match {
    case EsoLongestMatchParser(qs) => EsoLongestMatchParser(ps ++ qs)
    case _ => EsoLongestMatchParser(ps :+ q)
  }
  
  override def tramp[B](inp: String)(cc: EsoParseRes[A] => TailRec[EsoParseRes[B]]): TailRec[EsoParseRes[B]] = {
    def rec(pv: Vector[EsoParser[A]], maxMatch: EsoParseRes[A])(res: EsoParseRes[A]): TailRec[EsoParseRes[B]] = {
      val nxt = {
        if (res.length > maxMatch.length) res
        else maxMatch
      }
      pv match {
        case p +: pp => tailcall(p.tramp(inp)(rec(pp, nxt)))
        case _ => cc(maxMatch)
      }
    }
    
    tailcall(rec(ps, EsoParseFail)(EsoParseFail))
  }
}
