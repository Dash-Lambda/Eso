package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoConcatParser(parser1: => EsoParser[String], parser2: => EsoParser[String]) extends EsoParser[String]{
  private lazy val p = parser1
  private lazy val q = parser2
  
  def apply(inp: String): EsoParseRes[String] = applyByTramp(inp)
  
  override def tramp[AA >: String, B](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, B]): TailRec[ParseTrampResult[B]] = {
    tailcall(
      p.tramp(inp, start_ind)(
        pres =>
          pres.flatMapAll{
            case (pr, pi, ps, pe) =>
              q.tramp(pi, pe)(
                qres =>
                  qres.flatMapAll{
                    case (qr, qi, _, qe) =>
                      done(EsoParsedTramp(pr + qr, qi, ps, qe))})})) flatMap cc}
}
object EsoConcatParser{
  def apply(p: => EsoParser[String], q: => EsoParser[String]): EsoConcatParser = new EsoConcatParser(p, q)
}