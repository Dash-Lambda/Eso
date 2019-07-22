package interpreters

import spire.math.SafeLong

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object FracTran extends Interpreter{
  val name = "FracTran"
  
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String] = {
    getParms(bools, nums)("log", "powExp")("outputMaxLength") match{
      case Some((log +: powExp +: _, outputMaxLength +: _)) => Try{condition(progRaw)} match{
        case Success((init, prog)) =>
          def lst: LazyList[SafeLong] = evalLaz(prog)(init)
          def tran: LazyList[SafeLong] = if(outputMaxLength == -1) lst else lst.take(outputMaxLength)
          def res: LazyList[String] = tran.map{n =>
            if(powExp) s"<${FOP.factor(n).mkString(" ")}>"
            else n.toString
          }
          Success(res.map{str => if(log) println(str); str}.last)
        case Failure(e) => Failure(e)
      }
      case None => Failure(InterpreterException("Unspecified Runtime Parameters"))
    }
  }
  
  def evalLaz(prog: Vector[(SafeLong, SafeLong)])(init: SafeLong): LazyList[SafeLong] = init #:: LazyList
    .unfold(init){num =>
      prog.collectFirst{
        case (n, d) if num%d == 0 =>
          val nxt = n*(num/d)
          (nxt, nxt)
      }
    }
  
  def condition(progRaw: String): (SafeLong, Vector[(SafeLong, SafeLong)]) = {
    val vec = progRaw
      .filter("0123456789/\n".contains(_))
      .split("\n")
      .filter(_.nonEmpty)
      .map{_
        .split("/")
        .map(n => SafeLong(BigInt(n)))}
      .toVector
    
    def mkFrac(a: SafeLong, b: SafeLong): (SafeLong, SafeLong) = {
      val gcd = a.gcd(b)
      (a/gcd, b/gcd)
    }
    
    (vec.filter(_.sizeIs == 1).head.head, vec.filter(_.sizeIs == 2).map(v => mkFrac(v(0), v(1))))
  }
}
