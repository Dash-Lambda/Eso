package parsers

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

case class OrderedRegexParser[B](reg: Regex)(func: Match => B) extends OrderedParser[String, B]{
  def apply(inp: String): EsoParseRes[String, B] = reg.findFirstMatchIn(inp) match{
    case Some(m) => EsoParsed(func(m), m.after.toString, m.start, m.end)
    case None => EsoParseFail}
  
  override def parseValuesIterator(inp: String): Iterator[B] = reg.findAllMatchIn(inp) map func
  override def parseIterator(inp: String): Iterator[EsoParsed[String, B]] = {
    reg.findAllMatchIn(inp) map{m =>
      EsoParsed(func(m), m.after.toString, m.start, m.end)}}
  
  override def map[C](f: B => C): OrderedParser[String, C] = OrderedRegexParser(reg)(func andThen f)
  
  override def matches(inp: String): Boolean = reg.matches(inp)
}

object OrderedRegexParser{
  def apply[B](reg: String)(func: Match => B): OrderedRegexParser[B] = OrderedRegexParser(reg.r)(func)
}