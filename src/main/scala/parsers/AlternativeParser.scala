package parsers

case class AlternativeParser[A, B](ps: Vector[EsoParser[A, B]]) extends EsoParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = ps
    .iterator
    .map(p => p(inp))
    .find(_.passed)
    .getOrElse(EsoParseFail)
  
  override def |(q: EsoParser[A, B]): AlternativeParser[A, B] = q match{
    case AlternativeParser(qs) => AlternativeParser(ps :++ qs)
    case _ => AlternativeParser(ps :+ q)}
}
