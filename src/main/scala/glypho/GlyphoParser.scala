package glypho

import parsers.{EsoParser, RegexParser}

import scala.annotation.tailrec

object GlyphoParser {
  val normParse: EsoParser[String, Char] = {
    @tailrec
    def normalize(src: Seq[Char], ac: Vector[Int] = Vector(), maps: Vector[Char] = Vector()): String = src match{
      case c +: cs => maps.indexOf(c) match{
        case -1 => normalize(cs, ac :+ maps.size, maps :+ c)
        case n => normalize(cs, ac :+ n, maps)}
      case _ => ac.mkString}
    RegexParser("(....)")(m => normalize(m.group(1))) map {
      case "0000" => 'n'
      case "0001" => 'i'
      case "0010" => '>'
      case "0011" => '\\'
      case "0012" => '1'
      case "0100" => '<'
      case "0101" => 'd'
      case "0102" => '+'
      case "0110" => '['
      case "0111" => 'o'
      case "0112" => '*'
      case "0120" => 'e'
      case "0121" => '-'
      case "0122" => '!'
      case "0123" => ']'}}
  
  def parseAll(progRaw: String): Vector[Char] = normParse.parseAllValues(progRaw)
  def parseOne(tok: String): Char = normParse.parseOne(tok)
}
