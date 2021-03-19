package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoLImpParser[+A, +B](parser1: => EsoParser[A], parser2: => EsoParser[B]) extends EsoParser[A] {
  private lazy val p = parser1
  private lazy val q = parser2
  def apply(inp: String): EsoParseRes[A] = tramp(inp)(done).result
  
  override def tramp[C](inp: String)(cc: EsoParseRes[A] => TailRec[EsoParseRes[C]]): TailRec[EsoParseRes[C]] = {
    tailcall(
      p.tramp(inp) {
        case EsoParsed(tok, rem1, s1, e1) =>
          tailcall(
            q.tramp(rem1) {
              case EsoParsed(_, rem2, _, e2) => tailcall(cc(EsoParsed(tok, rem2, s1, e1 + e2)))
              case _ => tailcall(cc(EsoParseFail))})
        case _ => tailcall(cc(EsoParseFail))})}
}
object EsoLImpParser{
  def apply[A,B](p: => EsoParser[A], q: => EsoParser[B]): EsoLImpParser[A,B] = new EsoLImpParser(p, q)
}