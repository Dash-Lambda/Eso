package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoConstantParser[A,+B](parser: => EsoParser[A], v: => B) extends EsoParser[B]{
  private lazy val p = parser
  def apply(inp: String): EsoParseRes[B] = tramp(inp)(done).result
  
  override def ^^^[C](v2: => C): EsoParser[C] = EsoConstantParser(p, v2)
  
  override def tramp[C](inp: String)(cc: EsoParseRes[B] => TailRec[EsoParseRes[C]]): TailRec[EsoParseRes[C]] = {
    tailcall(
      p.tramp(inp) {
        case EsoParsed(_, rem, s, e) => tailcall(cc(EsoParsed(v, rem, s, e)))
        case _ => tailcall(cc(EsoParseFail))})}
}
object EsoConstantParser{
  def apply[A,B](p: => EsoParser[A], v: => B): EsoConstantParser[A,B] = new EsoConstantParser(p, v)
}