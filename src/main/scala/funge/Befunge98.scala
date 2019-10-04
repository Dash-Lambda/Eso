package funge

import common.{Config, Interpreter, Vec2D}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.Try

object Befunge98 extends Interpreter{
  val name: String = "Befunge-98"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    @tailrec
    def bdo(fips: Vector[FIP], prog: BF98Prog, inp: Seq[Char], idCnt: Int, halt: Boolean): Option[(String, (Vector[FIP], BF98Prog, Seq[Char], Int, Boolean))] = {
      if(halt) None
      else fips match{
        case ip +: ips => ip(prog, inp) match{
          case FIPCont(nProg, nInp, nFip) => bdo(ips :+ nFip, nProg, nInp, idCnt, halt)
          case FIPSplit(nProg, nInp, nFip1, nFip2) => bdo(ips :+ nFip2.setID(idCnt) :+ nFip1, nProg, nInp, idCnt + 1, halt)
          case FIPOut(str, nProg, nInp, nFip) => Some((str, (ips :+ nFip, nProg, nInp, idCnt, halt)))
          case FIPHalt(ret) => ret match{
            case Some(r) => Some((s"\nReturn: $r", (Vector(), prog, Seq(), 0, true)))
            case None => bdo(ips, prog, inp, idCnt, halt)
          }
        }
        case _ => None
      }
    }
    
    val initPos = Vec2D(-1, 0)
    val initDt = Vec2D(1, 0)
    
    Try{BF98Prog(progRaw.filter(_ != '\f'), config.cal, config.bool("bfDiv"))} map {prog =>
      inputs => LazyList.unfold((Vector(FIP(0, prog.getNextInd(initPos, initDt), initDt, Vec2D(0, 0), bs=false, Vector(LazyList.continually(0)), immutable.HashMap())), prog, inputs, 1: Int, false: Boolean)){
        case (fips, prg, in, id, halt) => bdo(fips, prg, in, id, halt)
      }.flatten
    }
  }
}
