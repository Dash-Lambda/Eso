package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

class EsoSortedAltParser[+A](parser1: => EsoParser[A], parser2: => EsoParser[A], comp: (EsoParseResTramp[A], EsoParseResTramp[A]) => Boolean) extends EsoParser[A] {
  private lazy val p = parser1
  private lazy val q = parser2
  
  def apply(inp: String): EsoParseRes[A] = applyByTramp(inp)
  
  override def tramp[AA >: A, B](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, B]): TailRec[ParseTrampResult[B]] = {
    tailcall(
      p.tramp(inp, start_ind)(
        pres =>
          tailcall(
            q.tramp(pres.inp, start_ind){
              qres =>
                def backtrack(f: EsoParseResTramp[A], s: EsoParseResTramp[A]): TailRec[EsoParseResTramp[B]] = {
                  tailcall(cc(f.withInp(qres.inp)) flatMap (res =>
                    res orElse tailcall(cc(s.withInp(qres.inp)))))}
                if(comp(pres, qres)) backtrack(pres, qres)
                else if(comp(qres, pres)) backtrack(qres, pres)
                else tailcall(cc(EsoParseFailTramp(qres.inp)))})))}
}
object EsoSortedAltParser{
  def apply[A](p: => EsoParser[A], q: => EsoParser[A])(comp: (EsoParseResTramp[A], EsoParseResTramp[A]) => Boolean): EsoSortedAltParser[A] =
    new EsoSortedAltParser(p, q, comp)
}
