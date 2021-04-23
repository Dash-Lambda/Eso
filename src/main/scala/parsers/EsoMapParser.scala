package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

class EsoMapParser[A, +B](parser: => EsoParser[A], fun: A => B) extends EsoParser[B] {
  private lazy val p = parser
  
  def apply(inp: String): EsoParseRes[B] = applyByTramp(inp)
  
  override def map[C](f: B => C): EsoParser[C] = EsoMapParser(p)(fun andThen f)
  
  override def tramp[AA >: B, C](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, C]): TailRec[ParseTrampResult[C]] = {
    tailcall(
      p.tramp(inp, start_ind)(res =>
        tailcall(cc(res map {case (r, i, s, e) => (fun(r), i, s, e)}))))}
}
object EsoMapParser{
  def apply[A,B](parser: => EsoParser[A])(fun: A => B): EsoMapParser[A,B] = new EsoMapParser(parser, fun)
}