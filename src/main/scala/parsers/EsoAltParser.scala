package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoAltParser[+A](parser1: => EsoParser[A], parser2: => EsoParser[A]) extends EsoParser[A] {
  private lazy val p = parser1
  private lazy val q = parser2
  
  def apply(inp: String): EsoParseRes[A] = applyByTramp(inp)
  
  override def tramp[AA >: A, B](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, B]): TailRec[ParseTrampResult[B]] = {
    def backtrack(arg: ParseTrampResult[B]): TailRec[ParseTrampResult[B]] = if(arg.passed) done(arg) else tailcall(q.tramp(inp, start_ind)(cc))
    tailcall(
      p.tramp(inp, start_ind){
        res =>
          if(res.passed) tailcall(cc(res) flatMap backtrack)
          else tailcall(q.tramp(inp, start_ind)(cc))})}
}
object EsoAltParser{
  def apply[A](p: => EsoParser[A], q: => EsoParser[A]): EsoAltParser[A] = new EsoAltParser(p, q)
}