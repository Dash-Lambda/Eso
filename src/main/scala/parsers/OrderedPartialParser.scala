package parsers

case class OrderedPartialParser[A, B](func: PartialFunction[A, (B, A, Int, Int)]) extends OrderedParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = func.lift(inp) match{
    case Some((res, rem, start, end)) => EsoParsed(res, rem, start, end)
    case None => EsoParseFail}
  
  override def map[C](f: B => C): OrderedPartialParser[A, C] = new OrderedPartialParser(func andThen {case (res, rem, start, end) => (f(res), rem, start, end)})
  
  override def matches(inp: A): Boolean = func.isDefinedAt(inp)
}

object OrderedPartialParser{
  def simple[A, B](func: PartialFunction[A, (B, A)]): OrderedPartialParser[A, B] = OrderedPartialParser(func andThen {case (b, a) => (b, a, 0, 0)})
}