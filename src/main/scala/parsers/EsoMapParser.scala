package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

case class EsoMapParser[A, +B](parser: EsoParser[A], fun: A => B) extends EsoParser[B] {
  def apply(inp: String): EsoParseRes[B] = tramp(inp)(done).result
  
  override def map[C](f: B => C): EsoParser[C] = EsoMapParser(parser, fun andThen f)
  
  override def tramp[C](inp: String)(cc: EsoParseRes[B] => TailRec[EsoParseRes[C]]): TailRec[EsoParseRes[C]] = {
    tailcall(
      parser.tramp(inp) {
        res =>
          cc(res map fun)
      })
  }
}
