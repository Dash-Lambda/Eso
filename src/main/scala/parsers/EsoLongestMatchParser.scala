package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoLongestMatchParser[+A](val ps: LazyList[EsoParser[A]]) extends EsoParser[A] {
  def apply(inp: String): EsoParseRes[A] = tramp(inp)(done).result
  
  override def |||[B >: A](q: => EsoParser[B]): EsoParser[B] = q match {
    case elm: EsoLongestMatchParser[B] => EsoLongestMatchParser(ps #::: elm.ps)
    case _ => EsoLongestMatchParser(q #:: ps)}
  
  override def tramp[B](inp: String)(cc: EsoParseRes[A] => TailRec[EsoParseRes[B]]): TailRec[EsoParseRes[B]] = {
    def rec(pv: LazyList[EsoParser[A]], maxMatch: EsoParseRes[A])(res: EsoParseRes[A]): TailRec[EsoParseRes[B]] = {
      val nxt = {
        if (res.length > maxMatch.length) res
        else maxMatch}
      pv match {
        case p #:: pp => tailcall(p.tramp(inp)(rec(pp, nxt)))
        case _ => tailcall(cc(nxt))}}
    tailcall(rec(ps, EsoParseFail)(EsoParseFail))}
}
object EsoLongestMatchParser{
  def apply[A](ps: LazyList[EsoParser[A]]): EsoLongestMatchParser[A] = new EsoLongestMatchParser(ps)
}