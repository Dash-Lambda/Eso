package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoProdParser[+A, +B](parser1: => EsoParser[A], parser2: => EsoParser[B]) extends EsoParser[(A, B)] {
  private lazy val p = parser1
  private lazy val q = parser2
  
  def apply(inp: String): EsoParseRes[(A, B)] = applyByTramp(inp)
  
  override def tramp[AA >: (A, B), C](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, C]): TailRec[ParseTrampResult[C]] = {
    tailcall(
      p.tramp(inp, start_ind)(
        pres =>
          pres.flatMapAll{
            case (pr, ps, pe) =>
              q.tramp(inp, pe)(
                qres =>
                  qres.flatMapAll{
                    case (qr, _, qe) => done(EsoParsedTramp((pr, qr), ps, qe))})})) flatMap cc}
}
object EsoProdParser{
  def apply[A,B](p: => EsoParser[A], q: => EsoParser[B]): EsoProdParser[A,B] = new EsoProdParser(p, q)
}