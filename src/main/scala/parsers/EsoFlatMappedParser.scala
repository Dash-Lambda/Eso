package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoFlatMappedParser[A, +B](parser: => EsoParser[A], f: A => EsoParser[B]) extends EsoParser[B] {
  private lazy val p = parser
  def apply(inp: String): EsoParseRes[B] = tramp(inp)(done).result
  
  override def tramp[C](inp: String)(cc: EsoParseRes[B] => TailRec[EsoParseRes[C]]): TailRec[EsoParseRes[C]] = {
    tailcall(
      p.tramp(inp) {
        case EsoParsed(res, rem, s1, e1) =>
          tailcall(
            f(res).tramp(rem) {
              case EsoParsed(res2, rem2, _, e2) => tailcall(cc(EsoParsed(res2, rem2, s1, e1 + e2)))
              case _ => tailcall(cc(EsoParseFail))})
        case EsoParseFail => tailcall(cc(EsoParseFail))})}
}
object EsoFlatMappedParser{
  def apply[A,B](parser: => EsoParser[A], f: A => EsoParser[B]): EsoFlatMappedParser[A,B] = new EsoFlatMappedParser(parser, f)
}