package parsers

case class AfterParser[A, B](parser: EsoParser[A, B]) extends EsoParser[A, A]{
  def apply(inp: A): EsoParseRes[A, A] = parser(inp) mapAll {case (_, rem, s, e) => (rem, rem, s, e)}
}
