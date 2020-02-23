package parsers

import scala.annotation.tailrec

case class PartialArbitraryScanParser[A, B](collect: Vector[B] => Option[B])(func: PartialFunction[(Seq[A], Vector[B]), (Seq[A], Vector[B])]) extends EsoParser[Seq[A], B]{
  def apply(inp: Seq[A]): EsoParseRes[Seq[A], B] = {
    @tailrec
    def pdo(src: Seq[A], ac: Vector[B]): EsoParseRes[Seq[A], B] = {
      //println(s"- Parsing: ${src.takeRight(10).mkString}")
      func.lift(src, ac) match{
        case Some((ns, na)) => pdo(ns, na)
        case None => collect(ac) match{
          case Some(res) => EsoParsed(res, src, 0, inp.size)
          case None => EsoParseFail}}
    }
    pdo(inp, Vector())}
}
