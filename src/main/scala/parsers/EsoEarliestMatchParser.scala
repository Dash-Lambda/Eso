package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoEarliestMatchParser[+A](val ps: LazyList[EsoParser[A]]) extends EsoParser[A] {
  def apply(inp: String): EsoParseRes[A] = tramp(inp)(done).result
  
  override def ||[B >: A](q: => EsoParser[B]): EsoParser[B] = q match {
    case eem: EsoEarliestMatchParser[B] => EsoEarliestMatchParser(ps #::: eem.ps)
    case _ => EsoEarliestMatchParser(q #:: ps)}
  
  override def tramp[B](inp: String)(cc: EsoParseRes[A] => TailRec[EsoParseRes[B]]): TailRec[EsoParseRes[B]] = {
    def rec(pv: LazyList[EsoParser[A]], firstMatch: EsoParseRes[A])(res: EsoParseRes[A]): TailRec[EsoParseRes[B]] = {
      val nxt = {
        (firstMatch, res) match{
          case (EsoParsed(_, _, s1, _), EsoParsed(_, _, s2, _)) =>
            if(s2 < s1) res
            else firstMatch
          case (EsoParseFail, _) => res
          case (_, EsoParseFail) => firstMatch
          case _ => firstMatch}}
      pv match {
        case p #:: pp => tailcall(p.tramp(inp)(rec(pp, nxt)))
        case _ => tailcall(cc(nxt))}}
    tailcall(rec(ps, EsoParseFail)(EsoParseFail))}
}
object EsoEarliestMatchParser{
  def apply[A](ps: LazyList[EsoParser[A]]): EsoEarliestMatchParser[A] = new EsoEarliestMatchParser(ps)
}