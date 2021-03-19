package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoAltParser[+A](val ps: LazyList[EsoParser[A]]) extends EsoParser[A] {
  def apply(inp: String): EsoParseRes[A] = tramp(inp)(done).result
  
  override def |[B >: A](q: => EsoParser[B]): EsoParser[B] = q match {
    case eap: EsoAltParser[B] => EsoAltParser(ps #::: eap.ps)
    case _ => EsoAltParser(ps #::: LazyList(q))} // Gotta be a better way to do lazy FIFO...
  
  override def tramp[B](inp: String)(cc: EsoParseRes[A] => TailRec[EsoParseRes[B]]): TailRec[EsoParseRes[B]] = {
    def rec(pv: LazyList[EsoParser[A]])(res: EsoParseRes[A]): TailRec[EsoParseRes[B]] = {
      if (res.passed) tailcall(cc(res))
      else pv match {
        case p #:: pp => tailcall(p.tramp(inp)(rec(pp)))
        case _ => tailcall(cc(EsoParseFail))}}
    tailcall(rec(ps)(EsoParseFail))}
}
object EsoAltParser{
  def apply[A](ps: LazyList[EsoParser[A]]): EsoAltParser[A] = new EsoAltParser(ps)
}