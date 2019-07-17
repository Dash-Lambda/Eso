package interpreters

import spire.math.SafeLong

import scala.util.{Failure, Success, Try}

object FracTran extends Interpreter{
  val name = "FracTran"
  def apply(log: Boolean, debug: Boolean, outputMaxLength: Int)(progRaw: String): Try[String] = Try{condition(progRaw)} match{
    case Success((init, prog)) =>
      def lst: LazyList[SafeLong] = evalLaz(prog)(init)
      def tran: LazyList[SafeLong] = if(outputMaxLength == -1) lst else lst.take(outputMaxLength)
      Success(tran.map{n => if(log) println(n); n}.last.toString)
    case Failure(e) => Failure(e)
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
    
    (vec.filter(_.sizeIs == 1).head.head, vec.filter(_.sizeIs == 2).map(v => (v(0), v(1))))
  }
}
