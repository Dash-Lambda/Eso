package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}
import scala.util.matching.Regex

case class EsoRegexParser(reg: Regex) extends EsoParser[String]{
  def apply(inp: String): EsoParseRes[String] = {
    reg.findFirstMatchIn(inp) match{
      case Some(m) =>
        if(m.groupCount > 0) EsoParsed(m.subgroups.mkString, m.after.toString, m.start, m.end)
        else EsoParsed(m.matched, m.after.toString, m.start, m.end)
      case _ => EsoParseFail}}
  
  override def tramp[B](inp: EsoParserInput, start_ind: Int)(cc: EsoParseResTramp[String] => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = {
    val matcher = reg.pattern.matcher(inp.str)
    matcher.region(start_ind, inp.length) // Yeah, this is a mutable state thing... Don't really have much choice. At least it's totally contained within this method. The alternative is to drop the start of the string every time, which makes the time complexity exponential.
    if(matcher.find) // *deep sigh*
      if (matcher.groupCount > 0) tailcall(cc(EsoParsedTramp((1 to matcher.groupCount).map(matcher.group).mkString, matcher.start, matcher.end)))
      else tailcall(cc(EsoParsedTramp(matcher.group(), matcher.start, matcher.end)))
    else tailcall(cc(EsoParseFailTramp))}
}