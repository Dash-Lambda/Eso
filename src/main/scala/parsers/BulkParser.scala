package parsers

case class BulkParser[A, +B](parser: EsoParser[A, B], minimum: Int) extends EsoParser[A, Vector[B]]{
  def apply(inp: A): EsoParseRes[A, Vector[B]] = {
    val matches = parser.parseAll(inp)
    val res = matches.map{case EsoParsed(r, _, _, _) => r}
    if(matches.sizeIs < minimum) EsoParseFail
    else matches match{
      case EsoParsed(_, _, start, _) +: _ :+ EsoParsed(_, rem, _, end) => EsoParsed(res, rem, start, end)
      case EsoParsed(_, rem, start, end) +: _ => EsoParsed(res, rem, start, end)
      case _ => EsoParsed(Vector(), inp, -1, -1)}}
}
