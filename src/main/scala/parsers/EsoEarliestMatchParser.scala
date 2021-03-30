package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

class EsoEarliestMatchParser[+A](val ps: LazyList[EsoParser[A]]) extends EsoParser[A] {
  def apply(inp: String): EsoParseRes[A] = applyByTramp(inp)
  
  override def ||[B >: A](q: => EsoParser[B]): EsoParser[B] = q match {
    case eem: EsoEarliestMatchParser[B] => EsoEarliestMatchParser(ps #::: eem.ps)
    case _ => EsoEarliestMatchParser(q #:: ps)}
  
  override def tramp[B](inp: EsoParserInput, start_ind: Int)(cc: EsoParseResTramp[A] => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = {
    def rec(pv: LazyList[EsoParser[A]], firstMatch: EsoParseResTramp[A])(res: EsoParseResTramp[A]): TailRec[EsoParseResTramp[B]] = {
      val nxt = {
        (firstMatch, res) match{
          case (EsoParsedTramp(_, s1, _), EsoParsedTramp(_, s2, _)) => if(s2 < s1) res else firstMatch
          case (EsoParseFailTramp, _) => res
          case (_, EsoParseFailTramp) => firstMatch}}
      pv match {
        case p #:: pp => tailcall(p.tramp(inp, start_ind)(rec(pp, nxt)))
        case _ => tailcall(cc(nxt))}}
    tailcall(rec(ps, EsoParseFailTramp)(EsoParseFailTramp))}
}
object EsoEarliestMatchParser{
  def apply[A](ps: LazyList[EsoParser[A]]): EsoEarliestMatchParser[A] = new EsoEarliestMatchParser(ps)
}