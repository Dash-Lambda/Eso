package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

class EsoProdParser[+A, +B](parser1: => EsoParser[A], parser2: => EsoParser[B]) extends EsoParser[(A, B)] {
  private lazy val p = parser1
  private lazy val q = parser2
  def apply(inp: String): EsoParseRes[(A, B)] = applyByTramp(inp)
  
  override def tramp[C](inp: EsoParserInput, start_ind: Int)(cc: EsoParseResTramp[(A, B)] => TailRec[EsoParseResTramp[C]]): TailRec[EsoParseResTramp[C]] = {
    tailcall(
      p.tramp(inp, start_ind){
        case EsoParsedTramp(tok1, s1, e1) =>
          tailcall(
            q.tramp(inp, e1) {
              case EsoParsedTramp(tok2, _, e2) => tailcall(cc(EsoParsedTramp((tok1, tok2), s1, e2)))
              case _ => tailcall(cc(EsoParseFailTramp))})
        case _ => tailcall(cc(EsoParseFailTramp))})}
}
object EsoProdParser{
  def apply[A,B](p: => EsoParser[A], q: => EsoParser[B]): EsoProdParser[A,B] = new EsoProdParser(p, q)
}