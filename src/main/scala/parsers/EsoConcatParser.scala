package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

class EsoConcatParser(parser1: => EsoParser[String], parser2: => EsoParser[String]) extends EsoParser[String]{
  private lazy val p = parser1
  private lazy val q = parser2
  
  def apply(inp: String): EsoParseRes[String] = applyByTramp(inp)
  
  override def tramp[AA >: String, B](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, B]): TailRec[ParseTrampResult[B]] = {
    tailcall(
      p.tramp(inp, start_ind){
        case EsoParsedTramp(ptok, ps, pe) =>
          tailcall(
            q.tramp(inp, pe){
              case EsoParsedTramp(qtok, _, qe) => tailcall(cc(EsoParsedTramp(ptok + qtok, ps, qe)))
              case EsoParseFailTramp => tailcall(cc(EsoParseFailTramp))})
        case EsoParseFailTramp => tailcall(cc(EsoParseFailTramp))})}
}
object EsoConcatParser{
  def apply(p: => EsoParser[String], q: => EsoParser[String]): EsoConcatParser = new EsoConcatParser(p, q)
}