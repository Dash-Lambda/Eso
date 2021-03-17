package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoProdParser[+A, +B](p: => EsoParser[A], q: => EsoParser[B]) extends EsoParser[(A, B)] {
  /*def apply(inp: String): EsoParseRes[(A, B)] = p(inp) match{
    case EsoParsed(rem, tok1) => q(rem) map (tok2 => (tok1, tok2))
    case _ => EsoParseFail}*/
  def apply(inp: String): EsoParseRes[(A, B)] = tramp(inp)(done).result
  
  override def tramp[C](inp: String)(cc: EsoParseRes[(A, B)] => TailRec[EsoParseRes[C]]): TailRec[EsoParseRes[C]] = {
    tailcall(
      p.tramp(inp) {
        case EsoParsed(tok1, rem1, s1, e1) =>
          tailcall(
            q.tramp(rem1) {
              case EsoParsed(tok2, rem2, _, e2) => cc(EsoParsed((tok1, tok2), rem2, s1, e1 + e2))
              case _ => cc(EsoParseFail)
            })
        case _ => cc(EsoParseFail)
      })
  }
}
