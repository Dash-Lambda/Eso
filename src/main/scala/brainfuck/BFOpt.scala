package brainfuck

import common.{Config, Interpreter}

import scala.annotation.tailrec
import scala.util.{Either, Try}

object BFOpt extends Interpreter{
  val name: String = "bfOpt"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    Try{(config.bool("dyn"), config.num("init"))}
      .flatMap{case (dyn, init) => BFOptimize(progRaw) map bfRun(init, dyn)}
  }
  
  def bfRun(init: Int, dyn: Boolean)(prog: Vector[(Char, Either[Int, BlkOp])]): Seq[Char] => LazyList[Char] = {
    @tailrec def scan(dat: Vector[Int], stp: Int, ind: Int): Int = if (dat(ind) != 0) scan(dat, stp, ind + stp) else ind
    
    @tailrec
    def nxt(pc: Int, dc: Int, tape: Vector[Int], inp: Seq[Char]): Option[(Char, (Int, Int, Vector[Int], Seq[Char]))] = prog(pc) match{
      case (op, Right(bop)) => op match{
        case 'u' => nxt(pc + 1, dc + bop.shift, bop.doOp(dc, tape), inp)
        case 'l' => nxt(pc + 1, dc, bop.doLoop(dc, tape), inp)
      }
      case (op, Left(num)) => op match{
        case 'm' => nxt(pc + 1, dc + num, tape, inp)
        case '/' => nxt(pc + 1, scan(tape, num, dc), tape, inp)
        case '[' => nxt(if(tape(dc) == 0) num else pc + 1, dc, tape, inp)
        case ']' => nxt(if(tape(dc) != 0) num else pc + 1, dc, tape, inp)
        case ',' => nxt(pc + 1, dc, tape.updated(dc, inp.head.toInt), inp.tail)
        case '.' => Some((tape(dc).toChar, (pc + 1, dc, tape, inp)))
        case 'e' => None
      }
    }
    
    @tailrec
    def nxtDyn(pc: Int, dc: Int, tape: Vector[Int], inp: Seq[Char]): Option[(Char, (Int, Int, Vector[Int], Seq[Char]))] = {
      def padif(ind: Int): Vector[Int] = if(tape.isDefinedAt(ind)) tape else tape.padTo(ind + 1, 0)
      def scand(ind: Int, skp: Int): (Int, Vector[Int]) = {
        @tailrec
        def sdo(i: Int): (Int, Vector[Int]) = {
          if(tape.isDefinedAt(i)){
            if(tape(i) == 0) (i, tape)
            else sdo(i + skp)
          }else (i, tape.padTo(i + 1, 0))
        }
        sdo(ind)
      }
      
      prog(pc) match{
        case (op, Right(bop)) => op match{
          case 'u' => nxtDyn(pc + 1, dc + bop.shift, bop.doOp(dc, padif(dc + bop.maxShift)), inp)
          case 'l' => nxtDyn(pc + 1, dc, bop.doLoop(dc, padif(dc + bop.maxShift)), inp)
        }
        case (op, Left(num)) => op match{
          case 'm' => nxtDyn(pc + 1, dc + num, padif(dc + num), inp)
          case '/' =>
            val (ndc, ntp) = scand(dc, num)
            nxtDyn(pc + 1, ndc, ntp, inp)
          case '[' => nxtDyn(if(tape(dc) == 0) num else pc + 1, dc, tape, inp)
          case ']' => nxtDyn(if(tape(dc) != 0) num else pc + 1, dc, tape, inp)
          case ',' => nxtDyn(pc + 1, dc, tape.updated(dc, inp.head.toInt), inp.tail)
          case '.' => Some((tape(dc).toChar, (pc + 1, dc, tape, inp)))
          case 'e' => None
        }
      }
    }
    
    if(dyn) inputs => LazyList.unfold((0: Int, 0: Int, Vector.fill(init)(0), inputs)){case (pc, dc, tape, inp) => nxtDyn(pc, dc, tape, inp)}
    else inputs => LazyList.unfold((0: Int, 0: Int, Vector.fill(init)(0), inputs)){case (pc, dc, tape, inp) => nxt(pc, dc, tape, inp)}
  }
}
