package parsers

import scala.collection.mutable
import scala.util.matching.Regex

case class EsoRegexParser(reg: Regex, memo: mutable.HashMap[String, EsoParseRes[String]] = mutable.HashMap()) extends EsoParser[String]{
  def apply(inp: String): EsoParseRes[String] = {
    reg.findFirstMatchIn(inp) match{
      case Some(m) =>
        if(m.groupCount > 0) EsoParsed(m.subgroups.mkString, m.after.toString, m.start, m.end)
        else EsoParsed(m.matched, m.after.toString, m.start, m.end)
      case _ => EsoParseFail}}
}