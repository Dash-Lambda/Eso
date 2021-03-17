package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoRImpParser[+A, +B](p: => EsoParser[A], q: => EsoParser[B]) extends EsoParser[B] {
  /*def apply(inp: String): EsoParseRes[B] = p(inp) match{
    case EsoParsed(rem, _) => q(rem)
    case _ => EsoParseFail}*/
  def apply(inp: String): EsoParseRes[B] = tramp(inp)(done).result
  
  override def tramp[C](inp: String)(cc: EsoParseRes[B] => TailRec[EsoParseRes[C]]): TailRec[EsoParseRes[C]] = {
    tailcall(
      p.tramp(inp) {
        case EsoParsed(_, rem, s1, e1) =>
          tailcall(
            q.tramp(rem) {
              case EsoParsed(tok2, rem2, _, e2) => tailcall(cc(EsoParsed(tok2, rem2, s1, e1 + e2)))
              case _ => tailcall(cc(EsoParseFail))
            })
        case _ => cc(EsoParseFail)
      })
  }
}
