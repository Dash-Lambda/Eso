package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

class EsoLImpParser[+A, +B](parser1: => EsoParser[A], parser2: => EsoParser[B]) extends EsoParser[A] {
  private lazy val p = parser1
  private lazy val q = parser2
  def apply(inp: String): EsoParseRes[A] = applyByTramp(inp)
  
  override def tramp[C](inp: EsoParserInput, start_ind: Int)(cc: EsoParseResTramp[A] => TailRec[EsoParseResTramp[C]]): TailRec[EsoParseResTramp[C]] = {
    tailcall(
      p.tramp(inp, start_ind)({
        case EsoParsedTramp(tok, s1, e1) =>
          tailcall(
            q.tramp(inp, e1)({
              case EsoParsedTramp(_, _, e2) => tailcall(cc(EsoParsedTramp(tok, s1, e2)))
              case _ => tailcall(cc(EsoParseFailTramp))}))
        case _ => tailcall(cc(EsoParseFailTramp))}))}
}
object EsoLImpParser{
  def apply[A,B](p: => EsoParser[A], q: => EsoParser[B]): EsoLImpParser[A,B] = new EsoLImpParser(p, q)
}