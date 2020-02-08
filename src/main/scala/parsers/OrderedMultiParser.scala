package parsers

case class OrderedMultiParser[A, B](parsers: Vector[OrderedParser[A, B]]) extends OrderedParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = {
    parsers
      .map(p => p(inp))
      .sortBy(r => r.start - r.end)
      .sortBy(_.start)
      .find(_.passed)
      .getOrElse(EsoParseFail)}
  
  override def map[C](f: B => C): OrderedParser[A, C] = OrderedMultiParser(parsers.map(_.map(f)))
  
  override def <+>(p: OrderedParser[A, B]): OrderedMultiParser[A, B] = p match{
    case OrderedMultiParser(ps) => OrderedMultiParser(parsers :++ ps)
    case _ => OrderedMultiParser(parsers :+ p)}
}
