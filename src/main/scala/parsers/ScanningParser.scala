package parsers

import scala.annotation.tailrec

case class RightScanningParser[A, B](collect: Vector[B] => Option[B])(func: (A, Vector[B]) => Vector[B]) extends EsoParser[Seq[A], B]{
  def apply(inp: Seq[A]): EsoParseRes[Seq[A], B] = {
    @tailrec
    def pdo(src: Seq[A], ac: Vector[B]): EsoParseRes[Seq[A], B] = src match{
      case as :+ a => pdo(as, func(a, ac))
      case _ =>
        if(ac.isEmpty) EsoParseFail
        else collect(ac) match{
          case Some(res) => EsoParsed(res, Seq(), 0, inp.size)
          case None => EsoParseFail}}
    pdo(inp, Vector())}
}

case class LeftScanningParser[A, B](collect: Vector[B] => Option[B])(func: (A, Vector[B]) => Vector[B]) extends EsoParser[Seq[A], B]{
  def apply(inp: Seq[A]): EsoParseRes[Seq[A], B] = {
    @tailrec
    def pdo(src: Seq[A], ac: Vector[B]): EsoParseRes[Seq[A], B] = src match{
      case a +: as => pdo(as, func(a, ac))
      case _ =>
        if(ac.sizeIs != 1) EsoParseFail
        else collect(ac) match{
          case Some(res) => EsoParsed(res, Seq(), 0, inp.size)
          case None => EsoParseFail}}
    pdo(inp, Vector())}
}