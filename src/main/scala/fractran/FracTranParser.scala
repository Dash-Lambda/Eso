package fractran

import common.{EsoObj, PrimeNumTools}
import common.PrimeNumTools.factor
import fractran.FracTranpp.{BRK, FOP, INP, JMP, MLT, MSC, OUT}
import parsers.{ChunkParser, EsoParser, PartialElementwiseParser, RegexParser}
import spire.math.SafeLong
import spire.implicits._

import scala.util.Try

object FracTranParser extends EsoObj{
  val primes: LazyList[SafeLong] = PrimeNumTools.birdPrimes.to(LazyList)
  val termParser: EsoParser[String, SafeLong] = {
    val elmParser = RegexParser(raw"""(\d+)""")(m => m.group(1).toInt).toBulk map (v => (primes zip v) map {case (p, e) => p**e} reduce (_*_))
    val vecParser = RegexParser(raw"""<([\d ]+)>""")(m => m.group(1))
    val numParser = RegexParser(raw"""(-?\d+)""")(m => SafeLong(BigInt(m.group(1))))
    (vecParser andThen elmParser) | numParser}
  val fracParser: EsoParser[String, (SafeLong, SafeLong)] = RegexParser(raw"""(?m)^(-?(?:<[\d ]+>|\d+))\/(-?(?:<[\d ]+>|\d+))$$""")(m => (termParser.parseOne(m.group(1)), termParser.parseOne(m.group(2))))
  val initParser: EsoParser[String, SafeLong] = RegexParser(raw"""(?m)^(-?(?:\d+|<[\d ]+>))$$""")(m => termParser.parseOne(m.group(1)))
  
  val fopParser: EsoParser[Seq[(SafeLong, SafeLong)], FOP] = {
    val zero = SafeLong(0)
    def getPat(gcd: SafeLong): Int = {
      val facs = factor(gcd)
      facs(facs.zipWithIndex.collect{case (p, i) if p != 0 => i}.head)}
    def facOut(a: SafeLong, b: SafeLong): (SafeLong, SafeLong) = {
      val gcd = a.abs.gcd(b.abs)
      (a/gcd, b/gcd)}
    PartialElementwiseParser[(SafeLong, SafeLong), FOP]{
      case (`zero`, `zero`) => BRK
      case (`zero`, d) if 0 < d && d < 5 => INP(d.toInt)
      case (n, `zero`) if 0 < n && n < 5 => OUT(n.toInt)
      case (n, d) if n < 0 || d < 0 => JMP(d.abs, n.abs.toInt)
      case (n, d) if n.gcd(d) == 1 => MLT(n, d)
      case (n, d) => facOut(n, d) match{
        case (n2, d2) =>
          val ind = n.gcd(d).toInt
          MSC(n2, d2, getPat(ind), ind)}}}
  
  val ftParser: EsoParser[String, (SafeLong, Vector[(SafeLong, SafeLong)])] = initParser ~ fracParser.toBulk
  val fppParser: EsoParser[String, (SafeLong, Vector[Vector[FOP]])] = {
    val zero = SafeLong(0)
    def getPat(gcd: SafeLong): Int = {
      val facs = factor(gcd)
      facs(facs.zipWithIndex.collect{case (p, i) if p != 0 => i}.head)}
    def facOut(a: SafeLong, b: SafeLong): (SafeLong, SafeLong) = {
      val gcd = a.abs.gcd(b.abs)
      (a/gcd, b/gcd)}
    val breakParser = ChunkParser[Seq[FOP], Vector[FOP]]{fops =>
      if(fops.isEmpty) None
      else{
        val ind = fops.indexWhere(_ == BRK)
        if(ind == -1) Some((fops.toVector, Seq(), 0, fops.length))
        else Some((fops.take(ind).toVector, fops.drop(ind + 1), 0, ind + 1))}}
    val opParser = fracParser.map{
      case (`zero`, `zero`) => BRK
      case (`zero`, d) if 0 < d && d < 5 => INP(d.toInt)
      case (n, `zero`) if 0 < n && n < 5 => OUT(n.toInt)
      case (n, d) if n < 0 || d < 0 => JMP(d.abs, n.abs.toInt)
      case (n, d) if n.gcd(d) == 1 => MLT(n, d)
      case (n, d) => facOut(n, d) match{
        case (n2, d2) =>
          val ind = n.gcd(d).toInt
          MSC(n2, d2, getPat(ind), ind)}}
    initParser ~ (opParser streamInto breakParser).toBulk}
  
  def parse(progRaw: String): Try[(SafeLong, Vector[(SafeLong, SafeLong)])] = ftParser(progRaw).toTry("Malformed Program")
  def parseFOP(progRaw: String): Try[(SafeLong, Vector[Vector[FOP]])] = fppParser(progRaw).toTry("Malformed Program")
}
