package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoAllParser[+A](parser: => EsoParser[A], num: Int) extends EsoParser[Vector[A]]{
  private lazy val p = parser
  
  def apply(inp: String): EsoParseRes[Vector[A]] = applyByTramp(inp)
  
  override def tramp[AA >: Vector[A], B](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, B]): TailRec[ParseTrampResult[B]] = {
    def backtrack(ac: Vector[EsoParsedTramp[A]])(res: ParseTrampResult[B]): TailRec[ParseTrampResult[B]] = {
      if(res.passed) done(res)
      else if(ac.sizeIs >= num) {
        def nxt: TailRec[EsoParseResTramp[B]] = tailcall(cc(EsoParsedTramp(ac map (_.parsed), res.inp, ac.headOption.map(_.start).getOrElse(0), ac.lastOption.map(_.end).getOrElse(0))))
        if(ac.nonEmpty) nxt flatMap backtrack(ac.dropRight(1))
        else nxt}
      else tailcall(cc(EsoParseFailTramp(res.inp)))}
    
    def rec(ac: Vector[EsoParsedTramp[A]])(res: ParseTrampResult[A]): TailRec[ParseTrampResult[B]] = res match{
      case res: EsoParsedTramp[A] => tailcall(p.tramp(res.inp, res.end)(rec(ac :+ res)))
      case fr: EsoParseFailTramp => backtrack(ac)(fr)}
    tailcall(p.tramp(inp, start_ind)(rec(Vector())))}
}
object EsoAllParser{
  def apply[A](parser: => EsoParser[A], num: Int): EsoAllParser[A] = new EsoAllParser(parser, num)
}