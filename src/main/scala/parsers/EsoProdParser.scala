package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoProdParser[+A, +B](parser1: => EsoParser[A], parser2: => EsoParser[B]) extends EsoParser[(A, B)] {
  private lazy val p = parser1
  private lazy val q = parser2
  def apply(inp: String): EsoParseRes[(A, B)] = tramp(inp)(done).result
  
  override def tramp[C](inp: String)(cc: EsoParseRes[(A, B)] => TailRec[EsoParseRes[C]]): TailRec[EsoParseRes[C]] = {
    tailcall(
      p.tramp(inp) {
        case EsoParsed(tok1, rem1, s1, e1) =>
          tailcall(
            q.tramp(rem1) {
              case EsoParsed(tok2, rem2, _, e2) => tailcall(cc(EsoParsed((tok1, tok2), rem2, s1, e1 + e2)))
              case _ => tailcall(cc(EsoParseFail))})
        case _ => tailcall(cc(EsoParseFail))})}
}
object EsoProdParser{
  def apply[A,B](p: => EsoParser[A], q: => EsoParser[B]): EsoProdParser[A,B] = new EsoProdParser(p, q)
}