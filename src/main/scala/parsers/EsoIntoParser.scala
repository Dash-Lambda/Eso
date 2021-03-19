package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoIntoParser[+A](parser1: => EsoParser[String], parser2: => EsoParser[A]) extends EsoParser[A] {
  private lazy val p = parser1
  private lazy val q = parser2
  def apply(inp: String): EsoParseRes[A] = tramp(inp)(done).result
  
  override def tramp[B](inp: String)(cc: EsoParseRes[A] => TailRec[EsoParseRes[B]]): TailRec[EsoParseRes[B]] = {
    tailcall(
      p.tramp(inp) {
        case EsoParsed(tok, rem, s1, e1) =>
          tailcall(
            q.tramp(tok) {
              case EsoParsed(tok2, _, _, _) => tailcall(cc(EsoParsed(tok2, rem, s1, e1)))
              case _ => tailcall(cc(EsoParseFail))})
        case _ => tailcall(cc(EsoParseFail))})}
}
object EsoIntoParser{
  def apply[A](p: => EsoParser[String], q: => EsoParser[A]): EsoIntoParser[A] = new EsoIntoParser(p, q)
}