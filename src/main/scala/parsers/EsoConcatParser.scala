package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

class EsoConcatParser(p: => EsoParser[String], q: => EsoParser[String]) extends EsoParser[String]{
  def apply(inp: String): EsoParseRes[String] = applyByTramp(inp)
  
  override def tramp[B](inp: EsoParserInput, start_ind: Int)(cc: EsoParseResTramp[String] => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = {
    tailcall(
      p.tramp(inp, start_ind){
        case EsoParseFailTramp => tailcall(cc(EsoParseFailTramp))
        case EsoParsedTramp(ptok, ps, pe) =>
          tailcall(
            q.tramp(inp, pe){
              case EsoParseFailTramp => tailcall(cc(EsoParseFailTramp))
              case EsoParsedTramp(qtok, _, qe) => tailcall(cc(EsoParsedTramp(ptok + qtok, ps, qe)))})})}
}
object EsoConcatParser{
  def apply(p: => EsoParser[String], q: => EsoParser[String]): EsoConcatParser = new EsoConcatParser(p, q)
}