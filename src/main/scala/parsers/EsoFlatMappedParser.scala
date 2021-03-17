package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

case class EsoFlatMappedParser[A, +B](parser: EsoParser[A], f: A => EsoParser[B]) extends EsoParser[B] {
  def apply(inp: String): EsoParseRes[B] = parser(inp) flatMapWithNext f
  
  override def tramp[C](inp: String)(cc: EsoParseRes[B] => TailRec[EsoParseRes[C]]): TailRec[EsoParseRes[C]] = {
    tailcall(
      parser.tramp(inp) {
        case EsoParsed(res, rem, s1, e1) =>
          tailcall(
            f(res).tramp(rem) {
              case EsoParsed(res2, rem2, _, e2) => tailcall(cc(EsoParsed(res2, rem2, s1, e1 + e2)))
              case _ => tailcall(cc(EsoParseFail))
            })
        case EsoParseFail => cc(EsoParseFail)
      })
  }
}
