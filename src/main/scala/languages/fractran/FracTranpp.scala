package languages.fractran

import common.{Config, Interpreter, PrimeNumTools}
import common.PrimeNumTools.factor
import spire.math.SafeLong
import spire.implicits._

import scala.annotation.tailrec
import scala.util.Try

object FracTranpp extends Interpreter{
  val name: String = "FracTran++"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = FracTranParser.fppParse(progRaw).toTry() flatMap{
    case ((initNum, prog), _, _, _) => Try{fppRun(initNum, prog)}}
  
  def fppRun(initNum: SafeLong, blk: Vector[Vector[FOP]]): Seq[Char] => LazyList[Char] = {
    val primes = PrimeNumTools.birdPrimes
    def collapse(exps: Seq[Int]): SafeLong = exps.zip(primes).map{case (e, p) => p**e}.reduce(_+_)
    def splitLine(inp: Seq[Char]): (String, Seq[Char]) = inp.span(_ != '\n') match{
      case (hd, tl) => (hd.mkString, tl.drop(1))}
    @tailrec
    def fdo(num: SafeLong, src: Vector[FOP], bid: Int, inp: Seq[Char], call: Call): Option[(String, (SafeLong, Vector[FOP], Int, Seq[Char], Call))] = src match{
      case op +: ops if op.canDo(num) => op match{
        case INP(ext) => ext match{
          case 3 => fdo(SafeLong(inp.head.toInt), blk(bid), bid, inp.tail, call)
          case _ => splitLine(inp) match{
            case (hd, tl) => ext match{
              case 1 => fdo(SafeLong(BigInt(hd)), blk(bid), bid, tl, call)
              case 2 => fdo(collapse(hd.split(" ").toVector.map(_.toInt)), blk(bid), bid, tl, call)
              case 4 => fdo(collapse(hd.map(_.toInt)), blk(bid), bid, tl, call)}}}
        case OUT(ext) =>
          val str = ext match{
            case 1 => num.toString
            case 2 => factor(num).mkString(" ")
            case 3 => num.toChar.toString
            case 4 => factor(num).map(_.toChar).mkString}
          Some((str, (num, ops, bid, inp, call)))
        case JMP(_, ind) => fdo(num, blk(ind), ind, inp, call)
        case MSC(_, _, pat, ext) =>
          lazy val numf = factor(num).padTo(ext + 1, 0)
          pat match{
            case 1 => splitLine(inp) match{
              case (hd, tl) => fdo(collapse(numf.updated(ext, hd.toInt)), blk(bid), bid, tl, call)}
            case 2 => Some((numf(ext).toString, (op*num, ops, bid, inp, call)))
            case 3 => fdo(collapse(numf.updated(ext, inp.head.toInt)), blk(bid), bid, inp.tail, call)
            case 4 => Some((numf(ext).toChar.toString, (op*num, ops, bid, inp, call)))
            case 5 => fdo(op*num, blk(numf(ext)), numf(ext), inp, PassCall(blk(bid), bid, call))
            case 6 => fdo(num, blk(numf(ext)), numf(ext), inp, call)
            case 7 => fdo(op*num, blk(bid), bid, inp, StatCall(num, ops, bid, call))
            case 8 => fdo(op*num, blk(numf(ext)), numf(ext), inp, StatCall(num, ops, bid, call))}
        case _ => fdo(op*num, blk(bid), bid, inp, call)}
      case _ +: ops => fdo(num, ops, bid, inp, call)
      case _ => call match{
        case PassCall(np, nb, cc) => fdo(num, np, nb, inp, cc)
        case StatCall(nn, np, nb, cc) => fdo(nn, np, nb, inp, cc)
        case _ => None}}
    inputs => LazyList.unfold((initNum, blk.head, 0: Int, inputs, RetCall: Call)){
      case (num, ops, bid, inp, call) => fdo(num, ops, bid, inp, call)}.flatten}
  
  trait Call
  object RetCall extends Call
  case class PassCall(prog: Vector[FOP], bid: Int, cc: Call) extends Call
  case class StatCall(num: SafeLong, prog: Vector[FOP], bid: Int, cc: Call) extends Call
  
  trait FOP{
    def canDo(num: SafeLong): Boolean
    def *(num: SafeLong): SafeLong}
  
  object BRK extends FOP{
    def canDo(num: SafeLong): Boolean = false
    def *(num: SafeLong): SafeLong = num}
  
  case class INP(ext: Int) extends FOP{
    def canDo(num: SafeLong): Boolean = true
    def *(num: SafeLong): SafeLong = num}
  
  case class OUT(ext: Int) extends FOP{
    def canDo(num: SafeLong): Boolean = true
    def *(num: SafeLong): SafeLong = num}
  
  case class JMP(d: SafeLong, ind: Int) extends FOP{
    def canDo(num: SafeLong): Boolean = num%d == 0
    def *(num: SafeLong): SafeLong = num}
  
  case class MSC(n: SafeLong, d: SafeLong, pat: Int, ext: Int) extends FOP{
    def canDo(num: SafeLong): Boolean = (num*n)%d == 0
    def *(num: SafeLong): SafeLong = (num*n)/d}
  
  case class MLT(n: SafeLong, d: SafeLong) extends FOP{
    def canDo(num: SafeLong): Boolean = (num*n)%d == 0
    def *(num: SafeLong): SafeLong = (num*n)/d}
}
