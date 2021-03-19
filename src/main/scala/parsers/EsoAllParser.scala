package parsers

import scala.util.control.TailCalls.{TailRec, done, tailcall}

class EsoAllParser[+A](parser: => EsoParser[A], num: Int) extends EsoParser[Vector[A]]{
  private lazy val p = parser
  def apply(inp: String): EsoParseRes[Vector[A]] = tramp(inp)(done).result
  
  override def tramp[B](inp: String)(cc: EsoParseRes[Vector[A]] => TailRec[EsoParseRes[B]]): TailRec[EsoParseRes[B]] = {
    def rec(beg: Int, ac: Vector[EsoParsed[A]])(res: EsoParseRes[A]): TailRec[EsoParseRes[B]] = res match{
      case EsoParsed(tok, rem, start, end) => tailcall(p.tramp(rem)(rec(beg + end, ac :+ EsoParsed(tok, rem, beg + start, beg + end))))
      case _ =>
        if(num <= 0)
          if(ac.nonEmpty) tailcall(cc(EsoParsed(ac map (_.parsed), ac.last.rem, ac.head.start, ac.last.end)))
          else tailcall(cc(EsoParsed(Vector(),inp, 0, 0)))
        else if(ac.sizeIs >= num) tailcall(cc(EsoParsed(ac map (_.parsed), ac.last.rem, ac.head.start, ac.last.end)))
        else tailcall(cc(EsoParseFail))}
    tailcall(p.tramp(inp)(rec(0, Vector())))}
}
object EsoAllParser{
  def apply[A](parser: => EsoParser[A], num: Int): EsoAllParser[A] = new EsoAllParser(parser, num)
}