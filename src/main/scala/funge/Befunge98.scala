package funge

import common.{Config, Interpreter, Vec2D}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.Try

object Befunge98 extends Interpreter{
  val name: String = "Befunge-98"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    @tailrec
    def bdo(fips: Vector[FIP], prog: BF98Prog, dat: BF98State, idCnt: Int, halt: Boolean): Option[(String, (Vector[FIP], BF98Prog, BF98State, Int, Boolean))] = {
      if(halt) None
      else fips match{
        case ip +: ips => ip(prog, dat) match{
          case FIPCont(nProg, nInp, nFip) => bdo(ips :+ nFip, nProg, nInp, idCnt, halt)
          case FIPSplit(nProg, nInp, nFip1, nFip2) => bdo(ips :+ nFip2.setID(idCnt) :+ nFip1, nProg, nInp, idCnt + 1, halt)
          case FIPOut(str, nProg, nInp, nFip) => Some((str, (ips :+ nFip, nProg, nInp, idCnt, halt)))
          case FIPHalt(ret) => ret match{
            case Some(r) => if(config.bool("bfRetCode")) Some((s"\nReturn: $r", (Vector(), prog, dat, 0, true))) else None
            case None => bdo(ips, prog, dat, idCnt, halt)}}
        case _ => None}}
    
    val initPos = Vec2D(-1, 0)
    val initDt = Vec2D(1, 0)
    
    Try{BF98Prog(progRaw.filter(_ != '\f'), config.bool("bfDiv"))} map {prog =>
      val initFIP = FIP(0, prog.getNextInd(initPos, initDt), initDt, Vec2D(0, 0), bs=false, Vector(FungeStack(Vector())), immutable.HashMap())
      inputs => LazyList.unfold((Vector(initFIP), prog, BF98State(config.times, inputs, config.rand, BF98FPData.default), 1: Int, false: Boolean)){
        case (fips, prg, in, id, halt) => bdo(fips, prg, in, id, halt)}
        .flatten}}
}
