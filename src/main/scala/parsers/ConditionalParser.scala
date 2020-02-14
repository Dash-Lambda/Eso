package parsers

case class ConditionalParser[A, B](parser: EsoParser[A, B], cond: B => Boolean) extends EsoParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = {
    val res = parser(inp)
    if(res.passed && cond(res.get)) res
    else EsoParseFail}
}
