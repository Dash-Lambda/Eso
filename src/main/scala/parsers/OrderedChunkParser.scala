package parsers

case class OrderedChunkParser[A, B](func: A => Option[(B, A, Int, Int)]) extends OrderedParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = func(inp) match{
    case Some((res, rem, start, end)) => EsoParsed(res, rem, start, end)
    case None => EsoParseFail}
  override def map[C](f: B => C): OrderedParser[A, C] = OrderedChunkParser(func andThen (_.map{case (res, rem, start, end) => (f(res), rem, start, end)}))
}

object OrderedChunkParser{
  def simple[A, B](func: A => Option[(B, A)]): OrderedChunkParser[A, B] = OrderedChunkParser(func andThen (_.map{case (b, a) => (b, a, 0, 0)}))
}
