package fractran

import common.{EsoExcep, EsoObj, PrimeNumTools}
import spire.math.SafeLong

import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

object FracTranParser extends EsoObj{
  val fracReg: Regex = raw"""([^/]+)/([^/]+)""".r
  val vecReg: Regex = raw"""<([\d ]+)>""".r
  val elmReg: Regex = raw"""\s*(\d+)((?: .*)?)""".r
  val numReg: Regex = raw"""(-?\d+)""".r
  
  def parse(progRaw: String): Try[(SafeLong, Vector[(SafeLong, SafeLong)])] = {
    def toNum(str: String): Option[SafeLong] = str match{
      case numReg(n) => Some(SafeLong(BigInt(n)))
      case vecReg(lst) =>
        val nums = Iterator.unfold(lst){
          case elmReg(n, tl) => Some((n.toInt, tl))
          case _ => None}
        val num = (PrimeNumTools.birdPrimes zip nums).map{case (p, e) => p**e}.reduce(_*_)
        Some(num)
      case _ => None}
    
    val initProg = filterChars(progRaw, "0123456789/-<>\n ")
    def initNum: Option[SafeLong] = initProg
      .linesIterator
      .collectFirst{
        case numReg(n) => SafeLong(BigInt(n))}
    def fracs: Iterator[(SafeLong, SafeLong)] = initProg
      .linesIterator
      .collect{
        case fracReg(n, d) => (toNum(n), toNum(d))}
      .collect{
        case (Some(n), Some(d)) => (n, d)}
    initNum match{
      case Some(n) => Success((n, fracs.toVector))
      case None => Failure(EsoExcep("No Initial Value"))}}
}
