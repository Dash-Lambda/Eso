package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

case class EsoConcatParser(parsers: LazyList[EsoParser[String]]) extends EsoParser[String]{
  def apply(inp: String): EsoParseRes[String] = applyByTramp(inp)
  
  override def tramp[B](inp: EsoParserInput, start_ind: Int)(cc: EsoParseResTramp[String] => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = {
    def rec(ps: LazyList[EsoParser[String]], ac: Vector[EsoParsedTramp[String]], pos: Int): TailRec[EsoParseResTramp[B]] = ps match{
      case p #:: pp => tailcall(
        p.tramp(inp, pos){
          case res: EsoParsedTramp[String] => tailcall(rec(pp, ac :+ res, res.end))
          case _ => tailcall(cc(EsoParseFailTramp))})
      case _ =>
        if(ac.nonEmpty) tailcall(cc(EsoParsedTramp(ac.map(_.parsed).mkString, ac.head.start, ac.last.end)))
        else tailcall(cc(EsoParsedTramp("", 0, 0)))}
    tailcall(rec(parsers, Vector(), start_ind))}
  
  override def <+>(q: => EsoParser[String])(implicit ev: EsoParser[String] <:< EsoParser[String]): EsoParser[String] = q match{
    case EsoConcatParser(parsers) => EsoConcatParser(parsers #::: parsers)
    case _ => EsoConcatParser(parsers #::: LazyList(q))}
}
