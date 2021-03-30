package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

class EsoLCondParser[+A, +B](parser1: => EsoParser[A], parser2: => EsoParser[B]) extends EsoParser[A] {
  private lazy val p = parser1
  private lazy val q = parser2
  def apply(inp: String): EsoParseRes[A] = applyByTramp(inp)
  
  override def tramp[C](inp: EsoParserInput, start_ind: Int)(cc: EsoParseResTramp[A] => TailRec[EsoParseResTramp[C]]): TailRec[EsoParseResTramp[C]] = {
    tailcall(
      p.tramp(inp, start_ind)({
        case res: EsoParsedTramp[A] =>
          tailcall(
            q.tramp(inp, res.end)(res2 =>
              if(res2.passed) tailcall(cc(res))
              else tailcall(cc(EsoParseFailTramp))))
        case _ => tailcall(cc(EsoParseFailTramp))}))}
}
object EsoLCondParser{
  def apply[A,B](p: => EsoParser[A], q: => EsoParser[B]): EsoLCondParser[A,B] = new EsoLCondParser(p, q)
}