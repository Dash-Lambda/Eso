package languages.fractran

import common.{EsoObj, PrimeNumTools}
import common.PrimeNumTools.factor
import languages.fractran.FracTranpp.{BRK, FOP, INP, JMP, MLT, MSC, OUT}
import parsers.EsoParser
import parsers.EsoParser._
import parsers.CombinatorFuncs._
import spire.math.SafeLong
import spire.implicits._

object FracTranParser extends EsoObj{
  val primes: LazyList[SafeLong] = PrimeNumTools.birdPrimes.to(LazyList)
  
  val termParse: EsoParser[SafeLong] = {
    def elmParse: EsoParser[SafeLong] = (R("""\d+""".r) ^^ (_.toInt)).* ^^ (v => (primes zip v) map {case (p, e) => p**e} reduce (_*_))
    def vecParse: EsoParser[String] = S("<") &> R("""[\d ]+""".r) <& S(">")
    def numParse: EsoParser[SafeLong] = R("""-?\d+""".r) ^^ (n => SafeLong(BigInt(n)))
    into(vecParse, elmParse) | numParse}
  
  val fracParse: EsoParser[(SafeLong, SafeLong)] = into(R("""(?m)^-?(?:<[\d ]+>|\d+)/-?(?:<[\d ]+>|\d+)$""".r), (termParse <& S("/")) <&> termParse)
  val initParse: EsoParser[SafeLong] = into(R("""(?m)^-?(?:\d+|<[\d ]+>)$""".r), termParse)
  
  val ftParse: EsoParser[(SafeLong, Vector[(SafeLong, SafeLong)])] = initParse <&> fracParse.*
  val fppParse: EsoParser[(SafeLong, Vector[Vector[FOP]])] = {
    val zero = SafeLong(0)
    val opParse: EsoParser[FOP] = fracParse ^^ {
      case (`zero`, `zero`) => BRK
      case (`zero`, d) if 0 < d && d < 5 => INP(d.toInt)
      case (n, `zero`) if 0 < n && n < 5 => OUT(n.toInt)
      case (n, d) if n < 0 || d < 0 => JMP(d.abs, n.abs.toInt)
      case (n, d) if n.gcd(d) == 1 => MLT(n, d)
      case (n, d) =>
        val gcd = n.gcd(d).toInt
        val ind = factor(gcd).indexWhere(_ != 0)
        MSC(n/gcd, d/gcd, factor(n/gcd)(ind), ind)}
    val breakParse: EsoParser[Vector[Vector[FOP]]] = opParse.* ^^ {
      vec =>
        Vector.unfold(vec){v =>
          if(v.isEmpty) None
          else v.span(_ != BRK) match{
            case (hd, tl) => Some((hd, tl.drop(1)))}}}
    initParse <&> breakParse}
}
