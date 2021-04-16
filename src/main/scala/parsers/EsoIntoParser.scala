package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

class EsoIntoParser[+A](parser1: => EsoParser[String], parser2: => EsoParser[A]) extends EsoParser[A] {
  private lazy val p = parser1
  private lazy val q = parser2
  
  def apply(inp: String): EsoParseRes[A] = applyByTramp(inp)
  
  override def tramp[AA >: A, B](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, B]): TailRec[ParseTrampResult[B]] = {
    tailcall(
      p.tramp(inp, start_ind)({
        case EsoParsedTramp(tok, s1, e1) =>
          tailcall(
            q.tramp(EsoParserInput(tok), 0) {
              case EsoParsedTramp(tok2, _, _) => tailcall(cc(EsoParsedTramp(tok2, s1, e1)))
              case EsoParseFailTramp => tailcall(cc(EsoParseFailTramp))})
        case EsoParseFailTramp => tailcall(cc(EsoParseFailTramp))}))}
}
object EsoIntoParser{
  def apply[A](p: => EsoParser[String], q: => EsoParser[A]): EsoIntoParser[A] = new EsoIntoParser(p, q)
}