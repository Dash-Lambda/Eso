package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoLImpParser[+A, +B](p: => EsoParser[A], q: => EsoParser[B]) extends EsoParser[A] {
  /*def apply(inp: String): EsoParseRes[A] = p(inp) match{
    case res: EsoParsed[A] if q.matches(res.rem) => res
    case _ => EsoParseFail}*/
  def apply(inp: String): EsoParseRes[A] = tramp(inp)(done).result
  
  override def tramp[C](inp: String)(cc: EsoParseRes[A] => TailRec[EsoParseRes[C]]): TailRec[EsoParseRes[C]] = {
    tailcall(
      p.tramp(inp) {
        case EsoParsed(tok, rem1, s1, e1) =>
          tailcall(
            q.tramp(rem1) {
              case EsoParsed(_, rem2, _, e2) => cc(EsoParsed(tok, rem2, s1, e1 + e2))
              case _ => cc(EsoParseFail)
            })
        case _ => cc(EsoParseFail)
      })
  }
}
