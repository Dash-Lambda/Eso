package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoLCondParser[+A, +B](p: => EsoParser[A], q: => EsoParser[B]) extends EsoParser[A] {
  /*def apply(inp: String): EsoParseRes[A] = p(inp) match{
    case res: EsoParsed[A] if q.matches(res.rem) => res
    case _ => EsoParseFail}*/
  def apply(inp: String): EsoParseRes[A] = tramp(inp)(done).result
  
  override def tramp[C](inp: String)(cc: EsoParseRes[A] => TailRec[EsoParseRes[C]]): TailRec[EsoParseRes[C]] = {
    tailcall(
      p.tramp(inp) {
        case res: EsoParsed[A] =>
          tailcall(
            q.tramp(res.rem) {
              case _: EsoParsed[B] => cc(res)
              case _ => cc(EsoParseFail)
            })
        case _ => cc(EsoParseFail)
      })
  }
}
