package parsers

case class SequentialParser[A, +B, +C](p: EsoParser[A, B], q: EsoParser[A, C]) extends EsoParser[A, (B, C)]{
  def apply(inp: A): EsoParseRes[A, (B, C)] = p(inp) flatMapAll {
    case (res1, rem1, s1, e1) => q(rem1) flatMapAll {
      case (res2, rem2, _, e2) => EsoParsed((res1, res2), rem2, s1, e1 + e2)}}
}
