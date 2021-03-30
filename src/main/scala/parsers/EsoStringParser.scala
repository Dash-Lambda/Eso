package parsers

import scala.util.control.TailCalls.{TailRec, tailcall}

case class EsoStringParser(str: String) extends EsoParser[String] {
  def apply(inp: String): EsoParseRes[String] = {
    if(inp.startsWith(str)) EsoParsed(str, inp.drop(str.length), 0, str.length)
    else EsoParseFail}
  
  override def tramp[B](inp: EsoParserInput, start_ind: Int)(cc: EsoParseResTramp[String] => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = {
    if(inp.str.startsWith(str, start_ind)) tailcall(cc(EsoParsedTramp(str, start_ind, start_ind + str.length)))
    else tailcall(cc(EsoParseFailTramp))}
}
