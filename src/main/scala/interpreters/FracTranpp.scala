package interpreters

import spire.math.SafeLong
import spire.implicits._

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

case class FOP(n: SafeLong, d: SafeLong, exp: Vector[Int], ext: Int, j: Boolean, i: Boolean, o: Boolean){
  val isBreak: Boolean = n == 0 && d == 0
  def canDo(num: SafeLong): Boolean = num%d == 0
  def pat: Int = if(exp.isDefinedAt(ext)) exp(ext) else 0
  def *(num: SafeLong): SafeLong = n*num/d
  
  override def toString: String = {
    val frac = s"$n/$d {${exp.mkString(", ")}} ($ext)"
    if(j) "Jump: " + frac
    else if(i) "Input: " + frac
    else if(o) "Output: " + frac
    else if(n == 0 && d == 0) "Break: " + frac
    else if(ext != -1) "Extension: " + frac
    else "Normal: " + frac
  }
}
object FOP{
  val primes: LazyList[Int] = 2 #:: LazyList.from(3, 2).filter{n => Iterator.range(3, n.sqrt + 1, 2).forall(n%_ != 0)}
  
  def make(n: SafeLong, d: SafeLong): Option[FOP] = {
    lazy val gcd = n.abs.gcd(d.abs)
    lazy val facs = factor(n, d)
    lazy val ind = getInd(gcd)
    
    if(n == 0 && 1 <= d && 4 >= d) Some(new FOP(n, d, Vector[Int](), d.toInt, false, true, false))
    else if(d == 0 && 1 <= n && 4 >= n) Some(new FOP(n, d, Vector[Int](), n.toInt, false, false, true))
    else if(n == 0 && d == 0) Some(new FOP(n, d, Vector[Int](), -1, false, false, false))
    else if(n == 0 || d == 0) None
    else if(n < 0 || d < 0) Some(new FOP(n.abs, d.abs, Vector[Int](), n.abs.toInt, true, false, false))
    else if(gcd == 1) Some(new FOP(n, d, facs, -1, false, false, false))
    else if(1 <= facs(ind) && 8 >= facs(ind)) Some(new FOP(n/gcd, d/gcd, facs, getInd(gcd), false, false, false))
    else None
  }
  
  def getInd(gcd: SafeLong): Int = factor(gcd).zipWithIndex.collect{case (p, i) if p != 0 => i}.head
  def factor(n: SafeLong, d: SafeLong): Vector[Int] = factor(n).zipAll(factor(d), 0, 0).map{case (a, b) => a - b}
  def factor(num: SafeLong): Vector[Int] = {
    def fdo(init: SafeLong, f: Int): (SafeLong, Int) = {
      val lst = LazyList.unfold(init){n => if(n%f == 0) Some((n/f, n/f)) else None}
      if(lst.nonEmpty) (lst.last, lst.length)
      else (init, 0)
    }
    @tailrec
    def fgo(n: SafeLong, ac: Vector[Int] = Vector[Int](), src: LazyList[Int] = primes): Vector[Int] = fdo(n, src.head) match{
      case (nxt, 0) if nxt == 1 => ac
      case (nxt, p) =>
        if(nxt == 1) ac :+ p
        else fgo(nxt, ac :+ p, src.tail)
    }
    
    fgo(num.abs)
  }
}

object FracTranpp extends Interpreter{
  val name: String = "FracTran++"
  val primes: LazyList[Int] = FOP.primes
  
  def apply(log: Boolean, debug: Boolean, outputMaxLength: Int)(progRaw: String): Try[String] = condition(progRaw) match{
    case Some((init, prog)) => Try{
      if(debug) println(s"$init\n${prog.map(_.mkString("\n")).mkString("\nBREAK\n")}")
      val (num, str) = eval(log, debug, outputMaxLength)(init, prog)
      s"$num\n$str"
    }
    case None => Failure(InterpreterException("Program Format Error"))
  }
  
  def eval(log: Boolean, debug: Boolean, outputMaxLength: Int)(init: SafeLong, blk: Vector[Vector[FOP]], initID: Int = 0): (SafeLong, String) = {
    def vecNum(vec: Vector[Int]): SafeLong = vec
      .zip(primes.map(SafeLong(_)))
      .map{case (e, p) => p**e}
      .reduce(_*_)
    @tailrec
    def opdo(num: SafeLong, src: Vector[FOP], bid: Int, res: String): (SafeLong, String) = {
      src match{
        case op +: ops =>
          if(debug) println(s"$op: $num")
          if(op.i) op.ext match{
            case 1 => opdo(SafeLong(BigInt(StdIn.readLine)), blk(bid), bid, res)
            case 2 => opdo(StdIn.readLine.split(" ").map(_.toInt).zip(primes).map{case (e, p) => SafeLong(p)**e}.reduce(_+_), blk(bid), bid, res)
            case 3 => opdo(SafeLong(StdIn.readLine.head.toInt), blk(bid), bid, res)
            case 4 => opdo(StdIn.readLine.toVector.map(_.toInt).zip(primes).map{case (e, p) => SafeLong(p)**e}.reduce(_+_), blk(bid), bid, res)
          }
          else if(op.o) {
            val str = op.ext match {
              case 1 => num.toString
              case 2 => FOP.factor(num).mkString(" ")
              case 3 => num.toChar.toString
              case 4 => FOP.factor(num).map(_.toChar).mkString
            }
            if (log) println(str)
            if ((outputMaxLength == -1) || ((res ++ str).sizeIs < outputMaxLength)) opdo(num, ops, bid, res ++ str)
            else (num, (res ++ str).take(outputMaxLength))
          }
          else if(op.canDo(num)) {
            if(op.j) opdo(num, blk(op.ext), op.ext, res)
            else if(op.ext == -1) opdo(op*num, blk(bid), bid, res)
            else{
              val numf = FOP.factor(num).padTo(op.ext + 1, 0)
              lazy val nump = numf(op.ext)
              op.pat match{
                case 1 => opdo(vecNum(numf.updated(op.ext, StdIn.readInt)), blk(bid), bid, res)
                case 2 => opdo(num, ops, bid, res ++ numf(op.ext).toString)
                case 3 => opdo(vecNum(numf.updated(op.ext, StdIn.readLine.head.toInt)), blk(bid), bid, res)
                case 4 => opdo(num, ops, bid, res + nump.toChar)
                case 5 =>
                  val (nxt, nrs) = eval(log, debug, outputMaxLength)(num, blk, nump)
                  opdo(nxt, blk(nump), bid, res ++ nrs)
                case 6 => opdo(num, blk(nump), nump, res)
              }
            }
          }
          else opdo(num, ops, bid, res)
        case _ => (num, res)
      }
    }
    
    opdo(init, blk.head, 0, "")
  }
  
  def condition(progRaw: String): Option[(SafeLong, Vector[Vector[FOP]])] = {
    def expNum(str: String): SafeLong = str
      .init.tail
      .split(" ")
      .map(_.toInt)
      .zip(primes.map(SafeLong(_)))
      .map{case (e, p) => p**e}
      .reduce(_*_)
    def mkNum(str: String): SafeLong = {
      if(str.startsWith("<") && str.endsWith(">")) expNum(str)
      else SafeLong(BigInt(str))
    }
    
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
        case _ => ac :+ tmp
      }
      pdo()
    }
    
    progNum.collectFirst{case v if v.sizeIs == 1 => v(0)} match{
      case Some(init) => Some((init, prog))
      case None => None
    }
  }
}
