package parsers

import scala.util.control.TailCalls.{TailRec, tailcall, done}

class EsoEarliestMatchParser[+A](val parsers: LazyList[EsoParser[A]]) extends EsoParser[A] {
  def apply(inp: String): EsoParseRes[A] = applyByTramp(inp)
  
  override def ||[B >: A](q: => EsoParser[B]): EsoParser[B] = q match {
    case eem: EsoEarliestMatchParser[B] => EsoEarliestMatchParser(parsers #::: eem.parsers)
    case _ => EsoEarliestMatchParser(q #:: parsers)}
  
  override def tramp[B](inp: EsoParserInput, start_ind: Int)(cc: EsoParseResTramp[A] => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = {
    def evalAll(psrc: LazyList[EsoParser[A]], ac: Vector[EsoParsedTramp[A]]): TailRec[EsoParseResTramp[B]] = psrc match{
      case p #:: ps =>
        tailcall(
          p.tramp(inp, start_ind){
            case res: EsoParsedTramp[A] =>
              val nac = ac.span(r => r.start < res.start) match {case (a, b) => (a :+ res) :++ b}
              tailcall(evalAll(ps, nac))
            case EsoParseFailTramp => tailcall(evalAll(ps, ac))})
      case _ => tailcall(backtrackRec(ac))}
    def backtrackRec(rsrc: Vector[EsoParsedTramp[A]]): TailRec[EsoParseResTramp[B]] = {
      rsrc match{
        case r +: rs =>
          tailcall(
            cc(r) flatMap (res => if(res.passed) done(res) else tailcall(backtrackRec(rs))))
        case _ => tailcall(cc(EsoParseFailTramp))}}
    tailcall(evalAll(parsers, Vector()))}
}
object EsoEarliestMatchParser{
  def apply[A](ps: LazyList[EsoParser[A]]): EsoEarliestMatchParser[A] = new EsoEarliestMatchParser(ps)
}