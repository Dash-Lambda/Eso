package glypho

import common.{Config, Translator}

import scala.util.Try

object GlyphoShorthand extends Translator{
  val name: String = "GlyphoShorthand"
  val baseLang: String = "Glypho"
  
  def unapply(config: Config)(prog: String): Try[String] = Try{GlyphoParser.parseAll(prog)} map (_.mkString)
  
  def apply(config: Config)(prog: String): Try[String] = Try{
    prog.flatMap{
      case 'n' => "aaaa"
      case 'i' => "aaab"
      case '>' => "aaba"
      case '\\' => "aabb"
      case '1' => "aabc"
      case '<' => "abaa"
      case 'd' => "abab"
      case '+' => "abac"
      case '[' => "abba"
      case 'o' => "abbb"
      case '*' => "abbc"
      case 'e' => "abca"
      case '-' => "abcb"
      case '!' => "abcc"
      case ']' => "abcd"
      case _ => ""}}
}
