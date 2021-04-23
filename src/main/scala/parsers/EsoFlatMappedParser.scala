package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoFlatMappedParser[A, +B](parser: => EsoParser[A], f: A => EsoParser[B]) extends EsoParser[B] {
  private lazy val p = parser
  
  def apply(inp: String): EsoParseRes[B] = applyByTramp(inp)
  
  override def tramp[AA >: B, C](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, C]): TailRec[ParseTrampResult[C]] = {
    tailcall(
      p.tramp(inp, start_ind)(
        pres =>
          pres.flatMap{
            case (pr, pi, ps, pe) =>
              f(pr).tramp(pi, pe)(
                fres =>
                  fres.flatMap{
                    case (fr, fi, _, fe) =>
                      done(EsoParsedTramp(fr, fi, ps, fe))})})) flatMap cc}
}
object EsoFlatMappedParser{
  def apply[A,B](parser: => EsoParser[A], f: A => EsoParser[B]): EsoFlatMappedParser[A,B] = new EsoFlatMappedParser(parser, f)
}