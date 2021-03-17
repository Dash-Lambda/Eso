package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

case class EsoAltParser[+A](ps: Vector[EsoParser[A]]) extends EsoParser[A] {
  def apply(inp: String): EsoParseRes[A] = tramp(inp)(done).result
  
  override def |[B >: A](q: => EsoParser[B]): EsoParser[B] = q match {
    case EsoAltParser(qs) => EsoAltParser(ps ++ qs)
    case _ => EsoAltParser(ps :+ q)
  }
  
  override def tramp[B](inp: String)(cc: EsoParseRes[A] => TailRec[EsoParseRes[B]]): TailRec[EsoParseRes[B]] = {
    def rec(pv: Vector[EsoParser[A]])(res: EsoParseRes[A]): TailRec[EsoParseRes[B]] = {
      if (res.passed) cc(res)
      else pv match {
        case p +: pp => tailcall(p.tramp(inp)(rec(pp)))
        case _ => cc(EsoParseFail)
      }
    }
    
    tailcall(rec(ps)(EsoParseFail))
  }
}
