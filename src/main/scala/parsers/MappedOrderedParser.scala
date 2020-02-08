package parsers

case class MappedOrderedParser[A, B, C](parser: OrderedParser[A, B], func: B => C) extends OrderedParser[A, C]{
  def apply(inp: A): EsoParseRes[A, C] = parser(inp) map func
  override def map[D](f: C => D): OrderedParser[A, D] = MappedOrderedParser(parser, func andThen f)
}