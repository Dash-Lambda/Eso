package parsers

case class PartialParser[A, B](func: PartialFunction[A, (B, A, Int, Int)]) extends EsoParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = func.lift(inp) match{
    case Some((res, rem, start, end)) => EsoParsed(res, rem, start, end)
    case None => EsoParseFail}
  
  override def map[C](f: B => C): PartialParser[A, C] = new PartialParser(func andThen {case (res, rem, start, end) => (f(res), rem, start, end)})
  
  override def matches(inp: A): Boolean = func.isDefinedAt(inp)
}

object PartialParser{
  def simple[A, B](func: PartialFunction[A, (B, A)]): PartialParser[A, B] = PartialParser(func andThen {case (b, a) => (b, a, 0, 0)})
}