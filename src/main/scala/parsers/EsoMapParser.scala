package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoMapParser[A, +B](parser: => EsoParser[A], fun: A => B) extends EsoParser[B] {
  private lazy val p = parser
  def apply(inp: String): EsoParseRes[B] = tramp(inp)(done).result
  
  override def map[C](f: B => C): EsoParser[C] = EsoMapParser(p, fun andThen f)
  
  override def tramp[C](inp: String)(cc: EsoParseRes[B] => TailRec[EsoParseRes[C]]): TailRec[EsoParseRes[C]] = {
    tailcall(
      p.tramp(inp) {res =>
        tailcall(cc(res map fun))})}
}
object EsoMapParser{
  def apply[A,B](parser: => EsoParser[A], fun: A => B): EsoMapParser[A,B] = new EsoMapParser(parser, fun)
}