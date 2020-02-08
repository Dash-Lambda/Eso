package parsers

case class ConditioningParser[A, B](parser: OrderedParser[A, B], func: A => A) extends OrderedParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = parser(func(inp))
  
  override def withConditioning(f: A => A): OrderedParser[A, B] = ConditioningParser(parser, f andThen func)
  override def parseIterator(inp: A): Iterator[EsoParsed[A, B]] = parser.parseIterator(func(inp))
}