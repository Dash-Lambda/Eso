package fractran

import common.{Config, Interpreter}
import spire.math.SafeLong

import scala.util.Try

object FracTran extends Interpreter{
  val name: String = "FracTran"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    FracTranParser.parse(progRaw) map{case (init, prog) => _ => fti(init, prog).flatMap(n => s"$n\n")}}
  
  def fti(init: SafeLong, prog: Vector[(SafeLong, SafeLong)]): LazyList[SafeLong] = init #:: LazyList.unfold(init){num =>
    prog.collectFirst{
      case (n, d) if num%d == 0 =>
        val nxt = n*num/d
        (nxt, nxt)}}
}
