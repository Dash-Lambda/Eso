package parsers

import scala.annotation.tailrec

case class PartialRightScanningParser[A, B](collect: Vector[B] => Option[B])(func: PartialFunction[(A, Vector[B]), Vector[B]]) extends EsoParser[Seq[A], B]{
  def apply(inp: Seq[A]): EsoParseRes[Seq[A], B] = {
    @tailrec
    def pdo(src: Seq[A], ac: Vector[B]): EsoParseRes[Seq[A], B] = src match{
      case as :+ a => func.lift(a, ac) match{
        case Some(nac) => pdo(as, nac)
        case None => EsoParseFail}
      case _ =>
        if(ac.isEmpty) EsoParseFail
        else collect(ac) match{
          case Some(res) => EsoParsed(res, Seq(), 0, inp.size)
          case None => EsoParseFail}}
    pdo(inp, Vector())}
}

case class PartialLeftScanningParser[A, B](collect: Vector[B] => Option[B])(func: PartialFunction[(A, Vector[B]), Vector[B]]) extends EsoParser[Seq[A], B]{
  def apply(inp: Seq[A]): EsoParseRes[Seq[A], B] = {
    @tailrec
    def pdo(src: Seq[A], ac: Vector[B]): EsoParseRes[Seq[A], B] = src match{
      case a +: as => func.lift(a, ac) match{
        case Some(nac) => pdo(as, nac)
        case None => EsoParseFail}
      case _ =>
        if(ac.isEmpty) EsoParseFail
        else collect(ac) match{
          case Some(res) => EsoParsed(res, Seq(), 0, inp.size)
          case None => EsoParseFail}}
    pdo(inp, Vector())}
}
