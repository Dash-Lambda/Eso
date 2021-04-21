package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

class EsoAllParser[+A](parser: => EsoParser[A], num: Int) extends EsoParser[Vector[A]]{
  private lazy val p = parser
  
  def apply(inp: String): EsoParseRes[Vector[A]] = applyByTramp(inp)
  
  override def tramp[AA >: Vector[A], B](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, B]): TailRec[ParseTrampResult[B]] = {
    def rec(ac: Vector[EsoParsedTramp[A]])(res: ParseTrampResult[A]): TailRec[ParseTrampResult[B]] = res match{
      case EsoParsedTramp(tok, ninp, start, end) => tailcall(p.tramp(inp, end)(rec(ac :+ EsoParsedTramp(tok, ninp, start, end))))
      case EsoParseFailTramp(ninp) =>
        if(ac.sizeIs >= num) tailcall(cc(EsoParsedTramp(ac map (_.parsed), ninp, ac.headOption.map(_.start).getOrElse(0), ac.lastOption.map(_.end).getOrElse(0))))
        else tailcall(cc(EsoParseFailTramp(ninp)))}
    tailcall(p.tramp(inp, start_ind)(rec(Vector())))}
}
object EsoAllParser{
  def apply[A](parser: => EsoParser[A], num: Int): EsoAllParser[A] = new EsoAllParser(parser, num)
}