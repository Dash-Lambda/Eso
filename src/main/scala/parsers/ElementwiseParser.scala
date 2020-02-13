package parsers

import scala.annotation.tailrec

case class ElementwiseParser[A, B](func: A => B) extends EsoParser[Seq[A], B]{
  def apply(inp: Seq[A]): EsoParseRes[Seq[A], B] = inp match{
    case a +: as => EsoParsed(func(a), as, 0, 1)
    case _ => EsoParseFail}
}

case class PartialElementwiseParser[A, B](func: PartialFunction[A, B]) extends EsoParser[Seq[A], B]{
  def apply(inp: Seq[A]): EsoParseRes[Seq[A], B] = {
    @tailrec
    def pdo(src: Seq[A], depth: Int = 1): EsoParseRes[Seq[A], B] = src match{
      case a +: as => func.lift(a) match{
        case Some(b) => EsoParsed(b, as, 0, depth)
        case None => pdo(as, depth + 1)}
      case _ => EsoParseFail}
    pdo(inp)}
}