package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

case class EsoIntoParser[+A](p: EsoParser[String], q: EsoParser[A]) extends EsoParser[A] {
  def apply(inp: String): EsoParseRes[A] = tramp(inp)(done).result
  
  override def tramp[B](inp: String)(cc: EsoParseRes[A] => TailRec[EsoParseRes[B]]): TailRec[EsoParseRes[B]] = {
    tailcall(
      p.tramp(inp) {
        case EsoParsed(tok, rem, s1, e1) =>
          tailcall(
            q.tramp(tok) {
              case EsoParsed(tok2, _, _, _) => cc(EsoParsed(tok2, rem, s1, e1))
              case _ => cc(EsoParseFail)
            })
        case _ => cc(EsoParseFail)
      })
  }
}
