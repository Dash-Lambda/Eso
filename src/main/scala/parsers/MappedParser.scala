package parsers

case class MappedParser[A, B, C](parser: EsoParser[A, B], func: B => C) extends EsoParser[A, C]{
  def apply(inp: A): EsoParseRes[A, C] = parser(inp) map func
  override def map[D](f: C => D): EsoParser[A, D] = MappedParser(parser, func andThen f)
}