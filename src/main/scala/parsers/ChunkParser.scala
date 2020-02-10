package parsers

case class ChunkParser[A, B](func: A => Option[(B, A, Int, Int)]) extends EsoParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = func(inp) match{
    case Some((res, rem, start, end)) => EsoParsed(res, rem, start, end)
    case None => EsoParseFail}
  override def map[C](f: B => C): EsoParser[A, C] = ChunkParser(func andThen (_.map{case (res, rem, start, end) => (f(res), rem, start, end)}))
}

object ChunkParser{
  def simple[A, B](func: A => Option[(B, A)]): ChunkParser[A, B] = ChunkParser(func andThen (_.map{case (b, a) => (b, a, 0, 0)}))
}
