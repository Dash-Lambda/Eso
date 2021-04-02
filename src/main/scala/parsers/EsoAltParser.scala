package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoAltParser[+A](p: => EsoParser[A], q: => EsoParser[A]) extends EsoParser[A] {
  def apply(inp: String): EsoParseRes[A] = applyByTramp(inp)
  
  override def tramp[B](inp: EsoParserInput, start_ind: Int)(cc: EsoParseResTramp[A] => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = {
    def backtrack(res: EsoParseResTramp[B]): TailRec[EsoParseResTramp[B]] = if(res.passed) done(res) else tailcall(q.tramp(inp, start_ind)(cc))
    tailcall(
      p.tramp(inp, start_ind){
        res =>
          if(res.passed) tailcall(cc(res) flatMap backtrack)
          else tailcall(q.tramp(inp, start_ind)(cc))})}
}
object EsoAltParser{
  def apply[A](p: => EsoParser[A], q: => EsoParser[A]): EsoAltParser[A] = new EsoAltParser(p, q)
}