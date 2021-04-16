package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

class EsoFlatMappedParser[A, +B](parser: => EsoParser[A], f: A => EsoParser[B]) extends EsoParser[B] {
  private lazy val p = parser
  
  def apply(inp: String): EsoParseRes[B] = applyByTramp(inp)
  
  override def tramp[AA >: B, C](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, C]): TailRec[ParseTrampResult[C]] = {
    tailcall(
      p.tramp(inp, start_ind)({
        case EsoParsedTramp(res, s1, e1) =>
          tailcall(
            f(res).tramp(inp, e1) {
              case EsoParsedTramp(res2, _, e2) => tailcall(cc(EsoParsedTramp(res2, s1, e2)))
              case EsoParseFailTramp => tailcall(cc(EsoParseFailTramp))})
        case EsoParseFailTramp => tailcall(cc(EsoParseFailTramp))}))}
}
object EsoFlatMappedParser{
  def apply[A,B](parser: => EsoParser[A], f: A => EsoParser[B]): EsoFlatMappedParser[A,B] = new EsoFlatMappedParser(parser, f)
}