package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoLCondParser[+A, +B](parser1: => EsoParser[A], parser2: => EsoParser[B]) extends EsoParser[A] {
  private lazy val p = parser1
  private lazy val q = parser2
  def apply(inp: String): EsoParseRes[A] = tramp(inp)(done).result
  
  override def tramp[C](inp: String)(cc: EsoParseRes[A] => TailRec[EsoParseRes[C]]): TailRec[EsoParseRes[C]] = {
    tailcall(
      p.tramp(inp) {
        case res: EsoParsed[A] =>
          tailcall(
            q.tramp(res.rem) {res2 =>
              if(res2.passed) tailcall(cc(res))
              else tailcall(cc(EsoParseFail))})
        case _ => tailcall(cc(EsoParseFail))})}
}
object EsoLCondParser{
  def apply[A,B](p: => EsoParser[A], q: => EsoParser[B]): EsoLCondParser[A,B] = new EsoLCondParser(p, q)
}