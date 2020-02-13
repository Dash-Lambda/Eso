package fractran

import common.{EsoObj, PrimeNumTools}
import common.PrimeNumTools.factor
import fractran.FracTranpp.{BRK, FOP, INP, JMP, MLT, MSC, OUT}
import parsers.{EsoParser, PartialParser, RegexParser}
import spire.math.SafeLong
import spire.implicits._

import scala.util.Try

object FracTranParser extends EsoObj{
  val primes: LazyList[SafeLong] = PrimeNumTools.birdPrimes.to(LazyList)
  val termParser: EsoParser[String, SafeLong] = {
    val elmParser = RegexParser(raw"""(\d+)""")(m => m.group(1).toInt).* map (v => (primes zip v) map {case (p, e) => p**e} reduce (_*_))
    val vecParser = RegexParser(raw"""<([\d ]+)>""")(m => m.group(1))
    val numParser = RegexParser(raw"""(-?\d+)""")(m => SafeLong(BigInt(m.group(1))))
    (vecParser >> elmParser) | numParser}
  val fracParser: EsoParser[String, (SafeLong, SafeLong)] = RegexParser(raw"""(?m)^(-?(?:<[\d ]+>|\d+))\/(-?(?:<[\d ]+>|\d+))$$""")(m => (termParser.parseOne(m.group(1)), termParser.parseOne(m.group(2))))
  val initParser: EsoParser[String, SafeLong] = RegexParser(raw"""(?m)^(-?(?:\d+|<[\d ]+>))$$""")(m => termParser.parseOne(m.group(1)))
  
  val ftParser: EsoParser[String, (SafeLong, Vector[(SafeLong, SafeLong)])] = initParser <&> fracParser.*
  val fppParser: EsoParser[String, (SafeLong, Vector[Vector[FOP]])] = {
    val zero = SafeLong(0)
    val breakParser = PartialParser[Seq[FOP], Vector[FOP]]{
      case fops if fops.nonEmpty =>
        val ind = fops.indexWhere(_ == BRK)
        if(ind == -1) (fops.toVector, Seq(), 0, fops.length)
        else (fops.take(ind).toVector, fops.drop(ind + 1), 0, ind + 1)}
    val opParser = fracParser.map{
      case (`zero`, `zero`) => BRK
      case (`zero`, d) if 0 < d && d < 5 => INP(d.toInt)
      case (n, `zero`) if 0 < n && n < 5 => OUT(n.toInt)
      case (n, d) if n < 0 || d < 0 => JMP(d.abs, n.abs.toInt)
      case (n, d) if n.gcd(d) == 1 => MLT(n, d)
      case (n, d) =>
        val gcd = n.gcd(d).toInt
        val facs = factor(gcd)
        val pat = facs(facs.zipWithIndex.collect{case (p, i) if p != 0 => i}.head)
        MSC(n/gcd, d/gcd, pat, gcd)}
    initParser <&> (opParser ~> breakParser).*}
  
  def parse(progRaw: String): Try[(SafeLong, Vector[(SafeLong, SafeLong)])] = ftParser(progRaw).toTry("Malformed Program")
  def parseFOP(progRaw: String): Try[(SafeLong, Vector[Vector[FOP]])] = fppParser(progRaw).toTry("Malformed Program")
}
