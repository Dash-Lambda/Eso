package fractran

import common.{Config, EsoExcep, Interpreter}
import spire.math.SafeLong

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object FracTran extends Interpreter{
  val name: String = "FracTran"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    condition(progRaw) map{case (init, prog) => _ => fti(init, prog).flatMap(n => s"$n\n")}
  }
  
  def fti(init: SafeLong, prog: Vector[(SafeLong, SafeLong)]): LazyList[SafeLong] = init #:: LazyList.unfold(init){num =>
    prog.collectFirst{
      case (n, d) if num%d == 0 =>
        val nxt = n*num/d
        (nxt, nxt)
    }
  }
  
  def condition(progRaw: String): Try[(SafeLong, Vector[(SafeLong, SafeLong)])] = {
    val lines = progRaw.filter("0123456789/\n".contains(_)).split("\n").toVector
    
    def mkFrac(ns: String, ds: String): (SafeLong, SafeLong) = {
      val n = SafeLong(BigInt(ns)).abs
      val d = SafeLong(BigInt(ds)).abs
      val gcd = n.gcd(d)
      (n/gcd, d/gcd)
    }
    
    @tailrec
    def cdo(ac: Vector[(SafeLong, SafeLong)], src: Vector[String]): Vector[(SafeLong, SafeLong)] = src match{
      case op +: ops => op.split('/').toVector match{
        case n +: d +: tail if tail.isEmpty => cdo(ac :+ mkFrac(n, d), ops)
        case _ => cdo(ac, ops)
      }
      case _ => ac
    }
    
    lines.collectFirst{case str if str.forall(_.isDigit) => SafeLong(BigInt(str))} match{
      case Some(num) => Success((num, cdo(Vector[(SafeLong, SafeLong)](), lines)))
      case None => Failure(EsoExcep("No Initial Value"))
    }
  }
}
