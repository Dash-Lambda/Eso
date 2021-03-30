package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

class EsoLongestMatchParser[+A](val ps: LazyList[EsoParser[A]]) extends EsoParser[A] {
  def apply(inp: String): EsoParseRes[A] = applyByTramp(inp)
  
  override def |||[B >: A](q: => EsoParser[B]): EsoParser[B] = q match {
    case elm: EsoLongestMatchParser[B] => EsoLongestMatchParser(ps #::: elm.ps)
    case _ => EsoLongestMatchParser(q #:: ps)}
  
  override def tramp[B](inp: EsoParserInput, start_ind: Int)(cc: EsoParseResTramp[A] => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = {
    def rec(pv: LazyList[EsoParser[A]], maxMatch: EsoParseResTramp[A])(res: EsoParseResTramp[A]): TailRec[EsoParseResTramp[B]] = {
      val nxt = {
        if (res.length > maxMatch.length) res
        else maxMatch}
      pv match {
        case p #:: pp => tailcall(p.tramp(inp, start_ind)(rec(pp, nxt)))
        case _ => tailcall(cc(nxt))}}
    tailcall(rec(ps, EsoParseFailTramp)(EsoParseFailTramp))}
}
object EsoLongestMatchParser{
  def apply[A](ps: LazyList[EsoParser[A]]): EsoLongestMatchParser[A] = new EsoLongestMatchParser(ps)
}