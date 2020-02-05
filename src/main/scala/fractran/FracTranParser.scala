package fractran

import common.{EsoExcep, EsoObj, OrderedParser, OrderedRegexParser, PrimeNumTools}
import spire.math.SafeLong

import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

object FracTranParser extends EsoObj{
  val fracReg: Regex = raw"""(?m)^(-?(?:<[\d ]+>|\d+))\/(-?(?:<[\d ]+>|\d+))$$""".r
  val initReg: Regex = raw"""(?m)^(-?(?:\d+|<[\d ]+>))$$""".r
  val vecReg: Regex = raw"""<([\d ]+)>""".r
  val elmReg: Regex = raw"""\s*(\d+)((?: .*)?)""".r
  val numReg: Regex = raw"""(-?\d+)""".r
  lazy val fracParser: OrderedParser[String, (SafeLong, SafeLong)] = {
    OrderedRegexParser[(SafeLong, SafeLong)](fracReg){m => (toNum(m.group(1)), toNum(m.group(2)))}
  }
  
  private def toNum(str: String): SafeLong = str match{
    case numReg(n) => SafeLong(BigInt(n))
    case vecReg(lst) =>
      val nums = Iterator.unfold(lst){
        case elmReg(n, tl) => Some((n.toInt, tl))
        case _ => None}
      (PrimeNumTools.birdPrimes zip nums).map{case (p, e) => p**e}.reduce(_*_)}
  
  def parse(progRaw: String): Try[(SafeLong, Vector[(SafeLong, SafeLong)])] = {
    initReg.findFirstIn(progRaw) match{
      case Some(init) => Success((toNum(init), fracParser.parseAllValues(progRaw)))
      case None => Failure(EsoExcep("Malformed Program"))}}
}
