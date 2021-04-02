package parsers

import scala.util.control.TailCalls.{TailRec, tailcall, done}

class EsoEarliestMatchParser[+A](p: => EsoParser[A], q: => EsoParser[A]) extends EsoParser[A] {
  def apply(inp: String): EsoParseRes[A] = applyByTramp(inp)
  
  override def tramp[B](inp: EsoParserInput, start_ind: Int)(cc: EsoParseResTramp[A] => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = {
    tailcall{
      p.tramp(inp, start_ind){
        pres =>
          tailcall(
            q.tramp(inp, start_ind){
              qres =>
                if(pres.start <= qres.start && pres.passed) tailcall(cc(pres) flatMap (res => if(res.passed) done(res) else tailcall(cc(qres))))
                else tailcall(cc(qres) flatMap (res => if(res.passed) done(res) else tailcall(cc(pres))))})}}}
}
object EsoEarliestMatchParser{
  def apply[A](p: => EsoParser[A], q: => EsoParser[A]): EsoEarliestMatchParser[A] = new EsoEarliestMatchParser(p, q)
}