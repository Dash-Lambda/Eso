package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

class EsoFlatMapResultParser[+A, C](parser: => EsoParser[A], fun: (A, EsoParserInput, Int, Int) => TailRec[EsoParseResTramp[C]]) extends EsoParser[C] {
  private lazy val p: EsoParser[A] = parser
  def apply(inp: String): EsoParseRes[C] = applyByTramp(inp)
  
  override def tramp[AA >: C, B](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, B]): TailRec[ParseTrampResult[B]] = {
    tailcall(
      p.tramp(inp, start_ind){
        pres =>
          pres flatMap {
            case (pr, pi, ps, pe) =>
              fun(pr, pi, ps, pe)}}) flatMap cc}
}
object EsoFlatMapResultParser{
  def apply[A, C](parser: => EsoParser[A])(cond: (A, EsoParserInput, Int, Int) => TailRec[EsoParseResTramp[C]]): EsoFlatMapResultParser[A, C] = new EsoFlatMapResultParser(parser, cond)
}