package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

class EsoAltParser[+A](val ps: LazyList[EsoParser[A]]) extends EsoParser[A] {
  def apply(inp: String): EsoParseRes[A] = applyByTramp(inp)
  
  override def |[B >: A](q: => EsoParser[B]): EsoParser[B] = q match {
    case eap: EsoAltParser[B] => EsoAltParser(ps #::: eap.ps)
    case _ => EsoAltParser(ps #::: LazyList(q))} // Gotta be a better way to do lazy FIFO...
  
  override def tramp[B](inp: EsoParserInput, start_ind: Int)(cc: EsoParseResTramp[A] => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = {
    def rec(pv: LazyList[EsoParser[A]])(res: EsoParseResTramp[A]): TailRec[EsoParseResTramp[B]] = {
      if (res.passed) tailcall(cc(res))
      else pv match {
        case p #:: pp => tailcall(p.tramp(inp, start_ind)(rec(pp)))
        case _ => tailcall(cc(EsoParseFailTramp))}}
    tailcall(rec(ps)(EsoParseFailTramp))}
}
object EsoAltParser{
  def apply[A](ps: LazyList[EsoParser[A]]): EsoAltParser[A] = new EsoAltParser(ps)
}