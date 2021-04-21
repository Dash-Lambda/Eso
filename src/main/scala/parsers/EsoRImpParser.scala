package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoRImpParser[+A, +B](parser1: => EsoParser[A], parser2: => EsoParser[B]) extends EsoParser[B] {
  private lazy val p = parser1
  private lazy val q = parser2
  
  def apply(inp: String): EsoParseRes[B] = applyByTramp(inp)
  
  override def tramp[AA >: B, C](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, C]): TailRec[ParseTrampResult[C]] = {
    tailcall(
      p.tramp(inp, start_ind)(
        pres =>
          pres.flatMapAll{
            case (_, pi, ps, pe) =>
              q.tramp(pi, pe)(
                qres =>
                  qres.flatMapAll{
                    case (qr, qi, _, qe) =>
                      done(EsoParsedTramp(qr, qi, ps, qe))})})) flatMap cc}
}
object EsoRImpParser{
  def apply[A,B](p: => EsoParser[A], q: => EsoParser[B]): EsoRImpParser[A,B] = new EsoRImpParser(p, q)
}