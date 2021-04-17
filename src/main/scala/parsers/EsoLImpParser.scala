package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoLImpParser[+A, +B](parser1: => EsoParser[A], parser2: => EsoParser[B]) extends EsoParser[A] {
  private lazy val p = parser1
  private lazy val q = parser2
  
  def apply(inp: String): EsoParseRes[A] = applyByTramp(inp)
  
  override def tramp[AA >: A, C](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, C]): TailRec[ParseTrampResult[C]] = {
    tailcall(
      p.tramp(inp, start_ind)(
        pres =>
          pres.flatMapAll{
            case (pr, ps, pe) =>
              q.tramp(inp, pe)(
                qres =>
                  done(
                    qres.flatMapAll{
                      case (_, _, qe) => EsoParsedTramp(pr, ps, qe)}))})) flatMap cc}
}
object EsoLImpParser{
  def apply[A,B](p: => EsoParser[A], q: => EsoParser[B]): EsoLImpParser[A,B] = new EsoLImpParser(p, q)
}