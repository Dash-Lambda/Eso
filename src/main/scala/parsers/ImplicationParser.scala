package parsers

case class LeftImplicationParser[A, +B, U](p: EsoParser[A, B], q: EsoParser[A, U]) extends EsoParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = {
    val res = p(inp)
    res match{
      case EsoParsed(_, rem, _, _) if q.matches(rem) => res
      case _ => EsoParseFail}}
}

case class RightImplicationParser[A, +B, C](p: EsoParser[A, B], q: EsoParser[A, C]) extends EsoParser[A, C]{
  def apply(inp: A): EsoParseRes[A, C] = p(inp) flatMapAll {
    case (_, rem, _, end) => q(rem) flatMapAll {
      case (nres, nrem, ns, ne) => EsoParsed(nres, nrem, end + ns, end + ne)}}
}