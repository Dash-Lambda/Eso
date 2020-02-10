package parsers

case class LongestMatchAlternativeParser[A, B](parsers: Vector[EsoParser[A, B]]) extends EsoParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = {
    parsers
      .map(p => p(inp))
      .sortBy(r => r.start - r.end)
      .sortBy(_.start)
      .find(_.passed)
      .getOrElse(EsoParseFail)}
  
  override def map[C](f: B => C): EsoParser[A, C] = LongestMatchAlternativeParser(parsers.map(_.map(f)))
  
  override def <+>(p: EsoParser[A, B]): LongestMatchAlternativeParser[A, B] = p match{
    case LongestMatchAlternativeParser(ps) => LongestMatchAlternativeParser(parsers :++ ps)
    case _ => LongestMatchAlternativeParser(parsers :+ p)}
}
