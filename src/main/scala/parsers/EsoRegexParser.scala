package parsers

import scala.util.matching.Regex

case class EsoRegexParser(reg: Regex) extends EsoParser[String]{
  def apply(inp: String): EsoParseRes[String] = {
    reg.findFirstMatchIn(inp) match{
      case Some(m) =>
        if(m.groupCount > 0) EsoParsed(m.subgroups.mkString, m.after.toString, m.start, m.end)
        else EsoParsed(m.matched, m.after.toString, m.start, m.end)
      case _ => EsoParseFail}}
}
