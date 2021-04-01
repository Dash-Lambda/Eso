package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoAltParser[+A](val parsers: LazyList[EsoParser[A]]) extends EsoParser[A] {
  def apply(inp: String): EsoParseRes[A] = applyByTramp(inp)
  
  override def |[B >: A](q: => EsoParser[B]): EsoParser[B] = q match {
    case eap: EsoAltParser[B] => EsoAltParser(parsers #::: eap.parsers)
    case _ => EsoAltParser(parsers #::: LazyList(q))} // Gotta be a better way to do lazy FIFO...
  
  override def tramp[B](inp: EsoParserInput, start_ind: Int)(cc: EsoParseResTramp[A] => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = {
    def rec(psrc: LazyList[EsoParser[A]]): TailRec[EsoParseResTramp[B]] = psrc match{
      case p +: ps =>
        tailcall(
          p.tramp(inp, start_ind){
            res =>
              if(res.passed) tailcall(cc(res) flatMap backtrack(ps))
              else tailcall(rec(ps))})
      case _ => tailcall(cc(EsoParseFailTramp))}
    def backtrack(psrc: LazyList[EsoParser[A]])(res: EsoParseResTramp[B]): TailRec[EsoParseResTramp[B]] = {
      if(res.passed) done(res)
      else tailcall(rec(psrc))}
    tailcall(rec(parsers))}
}
object EsoAltParser{
  def apply[A](ps: LazyList[EsoParser[A]]): EsoAltParser[A] = new EsoAltParser(ps)
}