package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoIntoParser[+A](parser1: => EsoParser[String], parser2: => EsoParser[A]) extends EsoParser[A] {
  private lazy val p = parser1
  private lazy val q = parser2
  
  def apply(inp: String): EsoParseRes[A] = applyByTramp(inp)
  
  override def tramp[AA >: A, B](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, B]): TailRec[ParseTrampResult[B]] = {
    tailcall(
      p.tramp(inp, start_ind)(
        pres =>
          pres.flatMapAll{
            case (pr, ps, pe) =>
              q.tramp(EsoParserInput(pr), 0)(
                qres =>
                  qres.flatMap(
                    qr => done(EsoParsedTramp(qr, ps, pe))))})) flatMap cc}
}
object EsoIntoParser{
  def apply[A](p: => EsoParser[String], q: => EsoParser[A]): EsoIntoParser[A] = new EsoIntoParser(p, q)
}