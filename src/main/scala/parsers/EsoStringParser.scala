package parsers

case class EsoStringParser(str: String) extends EsoParser[String] {
  def apply(inp: String): EsoParseRes[String] = {
    if(inp.startsWith(str)) EsoParsed(str, inp.drop(str.length), 0, str.length)
    else EsoParseFail}
}
