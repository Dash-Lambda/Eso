package fractran

import common.{EsoObj, PrimeNumTools}
import parsers.{EsoParser, RegexParser}
import spire.math.SafeLong

import scala.util.Try

object FracTranParser extends EsoObj{
  val fppParser: EsoParser[String, (SafeLong, Vector[(SafeLong, SafeLong)])] = {
    val elmParser = RegexParser(raw"""(\d+)""")(m => m.group(1).toInt).toBulk map (v => (PrimeNumTools.birdPrimes zip v) map {case (p, e) => p**e} reduce (_*_))
    val vecParser = RegexParser(raw"""<([\d ]+)>""")(m => m.group(1))
    val numParser = RegexParser(raw"""(-?\d+)""")(m => SafeLong(BigInt(m.group(1))))
    val termParser = (vecParser andThen elmParser) | numParser
    val fracParser = RegexParser(raw"""(?m)^(-?(?:<[\d ]+>|\d+))\/(-?(?:<[\d ]+>|\d+))$$""")(m => (termParser.parseOne(m.group(1)), termParser.parseOne(m.group(2))))
    val initParser = RegexParser(raw"""(?m)^(-?(?:\d+|<[\d ]+>))$$""")(m => termParser.parseOne(m.group(1)))
    initParser ~ fracParser.toBulk}
  
  def parse(progRaw: String): Try[(SafeLong, Vector[(SafeLong, SafeLong)])] = fppParser(progRaw).toTry("Malformed Program")
}
