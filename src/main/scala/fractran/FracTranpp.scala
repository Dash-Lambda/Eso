package fractran

import common.{Config, EsoExcep, Interpreter, PrimeNumTools}
import spire.implicits._
import spire.math.SafeLong

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.util.{Failure, Success, Try}

object FracTranpp extends Interpreter{
  val name: String = "FracTran++"
  val primes: LazyList[SafeLong] = PrimeNumTools.birdPrimes.to(LazyList)
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = Try{condition(progRaw)} flatMap{
    case Some((init, prog)) => Success(fti(init, prog))
    case None => Failure(EsoExcep("Malformed Program"))}
  
  def fti(init: SafeLong, blk: Vector[Vector[FOP]]): Seq[Char] => LazyList[Char] = {
    def collapse(exps: Seq[Int]): SafeLong = exps.zip(primes).map{case (e, p) => p**e}.reduce(_+_)
    def getLine(inp: Seq[Char]): String = inp.takeWhile(_ != '\n').mkString
    def dropLine(inp: Seq[Char]): Seq[Char] = {
      val (_, tl) = inp.span(_ == '\n')
      if(tl.nonEmpty) tl.tail
      else Seq[Char]()}
    
    @tailrec
    def nxt(num: SafeLong, src: Vector[FOP], bid: Int, calls: List[(Option[SafeLong], Vector[FOP], Int)], inp: Seq[Char]): Option[(String, (SafeLong, Vector[FOP], Int, List[(Option[SafeLong], Vector[FOP], Int)], Seq[Char]))] = src match{
      case op +: ops =>
        if(op.i) op.ext match{
          case 1 => nxt(SafeLong(BigInt(getLine(inp))), blk(bid), bid, calls, dropLine(inp))
          case 2 => nxt(collapse(ArraySeq.unsafeWrapArray(getLine(inp).split(" ").map(_.toInt))), blk(bid), bid, calls, dropLine(inp))
          case 3 => nxt(SafeLong(inp.head.toInt), blk(bid), bid, calls, inp.tail)
          case 4 => nxt(collapse(getLine(inp).map(_.toInt)), blk(bid), bid, calls, dropLine(inp))}
        else if(op.o){
          val str = op.ext match{
            case 1 => num.toString
            case 2 => FOP.factor(num).mkString(" ")
            case 3 => num.toChar.toString
            case 4 => FOP.factor(num).map(_.toChar).mkString}
          Some((str, (num, ops, bid, calls, inp)))}
        else if(op.canDo(num)){
          if(op.j) nxt(num, blk(op.ext), op.ext, calls, inp)
          else if(op.ext == -1) nxt(op*num, blk(bid), bid, calls, inp)
          else{
            lazy val nxnum = op*num
            lazy val numf = FOP.factor(num).padTo(op.ext + 1, 0)
            lazy val nump = numf(op.ext)
            op.pat match{
              case 1 => nxt(collapse(numf.updated(op.ext, getLine(inp).toInt)), blk(bid), bid, calls, dropLine(inp))
              case 2 => Some((numf(op.ext).toString, (nxnum, ops, bid, calls, inp)))
              case 3 => nxt(collapse(numf.updated(op.ext, inp.head.toInt)), blk(bid), bid, calls, inp.tail)
              case 4 => Some((nump.toChar.toString, (nxnum, ops, bid, calls, inp)))
              case 5 => nxt(nxnum, blk(nump), nump, (None, blk(bid), bid) +: calls, inp)
              case 6 => nxt(num, blk(nump), nump, calls, inp)
              case 7 => nxt(nxnum, blk(bid), bid, (Some(num), ops, bid) +: calls, inp)
              case 8 => nxt(nxnum, blk(nump), nump, (Some(num), ops, bid) +: calls, inp)}}}
        else nxt(num, ops, bid, calls, inp)
      case _ => calls match{
        case (cop, cprog, cbid) +: cs => cop match{
          case Some(cnum) => nxt(cnum, cprog, cbid, cs, inp)
          case None => nxt(num, cprog, cbid, cs, inp)}
        case _ => None}}
    inputs => LazyList.unfold((init, blk.head, 0: Int, List[(Option[SafeLong], Vector[FOP], Int)](), inputs)){
      case (num, ops, bid, calls, inp) => nxt(num, ops, bid, calls, inp)}.flatten}
  
  def condition(progRaw: String): Option[(SafeLong, Vector[Vector[FOP]])] = {
    def expNum(str: String): SafeLong = str
      .init.tail
      .split(" ")
      .map(_.toInt)
      .zip(primes)
      .map{case (e, p) => p**e}
      .reduce(_*_)
    
    def mkNum(str: String): SafeLong = {
      if(str.startsWith("<") && str.endsWith(">")) expNum(str)
      else SafeLong(BigInt(str))}
    
    val progNum = progRaw
      .filter("0123456789/< >-\n".contains(_))
      .split("\n")
      .map{line =>
        line
          .split("/")
          .map(s => Try{mkNum(s)})
          .collect{case Success(n) => n}
          .toVector}
      .filter(_.nonEmpty)
      .toVector
    lazy val progFop = progNum
      .collect{case n +: d +: _ => FOP.make(n, d)}
      .collect{case Some(fop) => fop}
    lazy val prog = {
      @tailrec
      def pdo(ac: Vector[Vector[FOP]] = Vector[Vector[FOP]](), tmp: Vector[FOP] = Vector[FOP](), src: Vector[FOP] = progFop): Vector[Vector[FOP]] = src match{
        case f +: fs =>
          if(f.isBreak) pdo(ac :+ tmp, Vector[FOP](), fs)
          else pdo(ac, tmp :+ f, fs)
        case _ => ac :+ tmp}
      pdo()}
    progNum.collectFirst{case v if v.sizeIs == 1 => v(0)}.map(init => (init, prog))}
  
  case class FOP(n: SafeLong, d: SafeLong, exp: Vector[Int], ext: Int, j: Boolean, i: Boolean, o: Boolean){
    val isBreak: Boolean = n == 0 && d == 0
    lazy val pat: Int = exp(ext)
    def canDo(num: SafeLong): Boolean = num%d == 0
    def *(num: SafeLong): SafeLong = n*num/d
  }
  object FOP{
    def make(n: SafeLong, d: SafeLong): Option[FOP] = {
      lazy val gcd = n.abs.gcd(d.abs)
      lazy val facs = factor(n, d)
      lazy val ind = getInd(gcd)
      
      if(n == 0 && 1 <= d && 4 >= d) Some(new FOP(n, d, Vector[Int](), d.toInt, false, true, false))
      else if(d == 0 && 1 <= n && 4 >= n) Some(new FOP(n, d, Vector[Int](), n.toInt, false, false, true))
      else if(n == 0 && d == 0) Some(new FOP(n, d, Vector[Int](), -1, false, false, false))
      else if(n == 0 || d == 0) None
      else if(n < 0 || d < 0) Some(new FOP(n.abs, d.abs, facs, n.abs.toInt, true, false, false))
      else if(gcd == 1) Some(new FOP(n, d, facs, -1, false, false, false))
      else if(1 <= facs(ind) && 8 >= facs(ind)) Some(new FOP(n/gcd, d/gcd, facs.padTo(ind + 1, 0), ind, false, false, false))
      else None}
    
    def getInd(gcd: SafeLong): Int = factor(gcd).zipWithIndex.collect{case (p, i) if p != 0 => i}.head
    def factor(n: SafeLong, d: SafeLong): Vector[Int] = factor(n).zipAll(factor(d), 0, 0).map{case (a, b) => a - b}
    def factor(num: SafeLong): Vector[Int] = {
      @tailrec
      def fdo(init: SafeLong, f: SafeLong, c: Int = 0): (SafeLong, Int) = {
        if (init % f == 0) fdo(init / f, f, c + 1)
        else (init, c)}
      @tailrec
      def fgo(n: SafeLong, ac: Vector[Int] = Vector[Int](), src: LazyList[SafeLong] = primes): Vector[Int] = fdo(n, src.head) match {
        case (nxt, 0) if nxt == 1 => ac
        case (nxt, p) =>
          if (nxt == 1) ac :+ p
          else fgo(nxt, ac :+ p, src.tail)}
      fgo(num.abs)}
  }
}
