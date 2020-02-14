package fractran

import common.{EsoObj, PrimeNumTools}
import common.PrimeNumTools.factor
import fractran.FracTranpp.{BRK, FOP, INP, JMP, MLT, MSC, OUT}
import parsers.{EsoParser, PartialParser, RegexParser}
import spire.math.SafeLong
import spire.implicits._

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
      case fops if fops.nonEmpty => fops.span(_ != BRK) match{
        case (hd, tl) => (hd.toVector, tl.drop(1), 0, if(tl.nonEmpty) hd.size + 1 else hd.size)}}
    val opParser = fracParser.map{
      case (`zero`, `zero`) => BRK
      case (`zero`, d) if 0 < d && d < 5 => INP(d.toInt)
      case (n, `zero`) if 0 < n && n < 5 => OUT(n.toInt)
      case (n, d) if n < 0 || d < 0 => JMP(d.abs, n.abs.toInt)
      case (n, d) if n.gcd(d) == 1 => MLT(n, d)
      case (n, d) =>
        val gcd = n.gcd(d).toInt
        val ind = factor(gcd).indexWhere(_ != 0)
        val pat = factor(n/gcd)(ind)
        MSC(n/gcd, d/gcd, pat, ind)}
    initParser <&> (opParser ~> breakParser).*}
}
