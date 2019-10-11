package wierd

import common.{Config, Interpreter, Vec2D}
import funge.BF98Prog

import scala.annotation.tailrec
import scala.util.{Success, Try}

object Wierd extends Interpreter{
  val name: String = "Wierd"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    Try{WierdProg(progRaw)} map wierdRun
  }
  
  def wierdRun(initProg: WierdProg): Seq[Char] => LazyList[Char] = {
    @tailrec
    def wri(wips: Vector[WIP], inp: Seq[Char], prog: WierdProg): Option[(Char, (Vector[WIP], Seq[Char], WierdProg))] = wips match{
      case ip +: ips => ip(inp, prog) match{
        case WIPCont(nip, ninp, nprog) => wri(ips :+ nip, ninp, nprog)
        case WIPSplit(nip1, nip2) => wri(ips :+ nip2 :+ nip1, inp, prog)
        case WIPOut(c, nip) => Some((c, (ips :+ nip, inp, prog)))
        case WIPHalt => None
      }
    }
    
    val initWip = WIP(Vec2D(1, 1), Vec2D(1, 1), LazyList[Int]())
    
    inputs => LazyList.unfold((Vector(initWip), inputs, initProg)){
      case (wips, inp, prog) => wri(wips, inp, prog)
    }
  }
}
