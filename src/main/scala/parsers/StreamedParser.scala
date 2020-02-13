package parsers

case class StreamedParser[A, B, C](p: EsoParser[A, B], q: EsoParser[Seq[B], C]) extends EsoParser[A, C]{
  def apply(inp: A): EsoParseRes[A, C] = {
    val pres = p.parseAllLazy(inp)
    q(pres.map(_.get)) flatMapAll{
      case (res, _, qs, qe) =>
        pres(qe - 1) mapAll{
        case (_, rem, _, pe) => (res, rem, pres(qs).start, pe)}}}
}
