package parsers

case class ConditioningParser[A, B](parser: EsoParser[A, B], func: A => A) extends EsoParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = parser(func(inp))
  
  override def withConditioning(f: A => A): EsoParser[A, B] = ConditioningParser(parser, f andThen func)
  override def parseIterator(inp: A): Iterator[EsoParsed[A, B]] = parser.parseIterator(func(inp))
}