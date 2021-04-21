package parsers

import scala.util.control.TailCalls.{TailRec, tailcall, done}

class EsoLCondParser[+A, +B](parser1: => EsoParser[A], parser2: => EsoParser[B]) extends EsoParser[A] {
  private lazy val p = parser1
  private lazy val q = parser2
  
  def apply(inp: String): EsoParseRes[A] = applyByTramp(inp)
  
  override def tramp[AA >: A, C](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, C]): TailRec[ParseTrampResult[C]] = {
    tailcall(
      p.tramp(inp, start_ind)(
        pres =>
          pres.flatMapAll{
            case (pr, pi, ps, pe) =>
              q.tramp(pi, pe)(
                qres =>
                  qres.flatMapAll{
                    case (_, qi, _, _) =>
                      done(EsoParsedTramp(pr, qi, ps, pe))})})) flatMap cc}
}
object EsoLCondParser{
  def apply[A,B](p: => EsoParser[A], q: => EsoParser[B]): EsoLCondParser[A,B] = new EsoLCondParser(p, q)
}