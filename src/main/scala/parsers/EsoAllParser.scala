package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

class EsoAllParser[+A](parser: => EsoParser[A], num: Int) extends EsoParser[Vector[A]]{
  private lazy val p = parser
  def apply(inp: String): EsoParseRes[Vector[A]] = applyByTramp(inp)
  
  override def tramp[B](inp: EsoParserInput, start_ind: Int)(cc: EsoParseResTramp[Vector[A]] => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = {
    def rec(ac: Vector[EsoParsedTramp[A]])(res: EsoParseResTramp[A]): TailRec[EsoParseResTramp[B]] = res match{
      case EsoParsedTramp(tok, start, end) => tailcall(p.tramp(inp, end)(rec(ac :+ EsoParsedTramp(tok, start, end))))
      case _ =>
        if(ac.nonEmpty && ac.sizeIs >= num) tailcall(cc(EsoParsedTramp(ac map (_.parsed), ac.head.start, ac.last.end)))
        else if(num == 0 && ac.isEmpty) tailcall(cc(EsoParsedTramp(Vector(), 0, 0)))
        else tailcall(cc(EsoParseFailTramp))}
    tailcall(p.tramp(inp, start_ind)(rec(Vector())))}
}
object EsoAllParser{
  def apply[A](parser: => EsoParser[A], num: Int): EsoAllParser[A] = new EsoAllParser(parser, num)
}