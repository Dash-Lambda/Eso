package brainfuck

import common.{Config, Interpreter, MemTape}

import scala.annotation.tailrec
import scala.util.Try

object BFOpt extends Interpreter{
  val name: String = "bfOpt"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    Try{(config.bool("dyn"), config.num("init"))}
      .flatMap{case (dyn, init) => BFOptimize(progRaw) map bfRun(init, dyn)}}
  
  def bfRun(init: Int, dyn: Boolean)(prog: Vector[BFOp]): Seq[Char] => LazyList[Char] = {
    @tailrec def scan(dat: MemTape[Int], stp: Int, ind: Int): Int = if(dat(ind) != 0) scan(dat, stp, ind + stp) else ind
        
        @tailrec
        def nxt(pc: Int, tp: Int, tape: MemTape[Int], inp: Seq[Char]): Option[(String, (Int, Int, MemTape[Int], Seq[Char]))] = prog(pc) match{
          case BFMove(n) => nxt(pc + 1, tp + n, tape, inp)
          case BFScan(n) => nxt(pc + 1, scan(tape, n, tp), tape, inp)
          case BFIn => nxt(pc + 1, tp, tape.set(tp, inp.head.toInt), inp.tail)
          case BFOut(n) => Some((tape(tp).toChar.toString * n, (pc + 1, tp, tape, inp)))
          case BFOpenLoop(i) => nxt(if(tape(tp) == 0) i else pc + 1, tp, tape, inp)
          case BFCloseLoop(i) => nxt(if(tape(tp) != 0) i else pc + 1, tp, tape, inp)
          case BFEnd => None
          case bop: BlkOp => nxt(pc + 1, tp + bop.shift, bop(tp, tape), inp)}
    
    inputs => LazyList.unfold((0: Int, 0: Int, MemTape(Vector.fill(init)(0), dyn, 0), inputs)){case (pc, dc, tape, inp) => nxt(pc, dc, tape, inp)}.flatten}
}
