package parsers

case class ChainedParser[A, B, C](p: EsoParser[A, B], q: EsoParser[B, C]) extends EsoParser[A, C]{
  def apply(inp: A): EsoParseRes[A, C] = p(inp) flatMapAll{
    case (pass, rem, s, e) => q(pass) mapAll{
      case (res, _, _, _) => (res, rem, s, e)}}
}
