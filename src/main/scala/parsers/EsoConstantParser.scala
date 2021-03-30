package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

class EsoConstantParser[A,+B](parser: => EsoParser[A], v: => B) extends EsoParser[B]{
  private lazy val p = parser
  def apply(inp: String): EsoParseRes[B] = applyByTramp(inp)
  
  override def ^^^[C](v2: => C): EsoParser[C] = EsoConstantParser(p, v2)
  
  override def tramp[C](inp: EsoParserInput, start_ind: Int)(cc: EsoParseResTramp[B] => TailRec[EsoParseResTramp[C]]): TailRec[EsoParseResTramp[C]] = {
    tailcall(
      p.tramp(inp, start_ind)({
        case EsoParsedTramp(_, s, e) => tailcall(cc(EsoParsedTramp(v, s, e)))
        case _ => tailcall(cc(EsoParseFailTramp))}))}
}
object EsoConstantParser{
  def apply[A,B](p: => EsoParser[A], v: => B): EsoConstantParser[A,B] = new EsoConstantParser(p, v)
}