package brainfuck

import common.{Config, Interpreter, MemTape}

import scala.annotation.tailrec
import scala.util.{Either, Try}

object BFOpt extends Interpreter{
  val name: String = "bfOpt"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    Try{(config.bool("dyn"), config.num("init"))}
      .flatMap{case (dyn, init) => BFOptimize(progRaw) map bfRun(init, dyn)}}
  
  def bfRun(init: Int, dyn: Boolean)(prog: Vector[(Char, Either[Int, BlkOp])]): Seq[Char] => LazyList[Char] = {
    @tailrec def scan(dat: MemTape[Int], stp: Int, ind: Int): Int = if(dat(ind) != 0) scan(dat, stp, ind + stp) else ind
    
    @tailrec
    def nxt(pc: Int, dc: Int, tape: MemTape[Int], inp: Seq[Char]): Option[(Char, (Int, Int, MemTape[Int], Seq[Char]))] = prog(pc) match{
      case (op, Right(bop)) => op match{
        case 'u' => nxt(pc + 1, dc + bop.shift, bop.doOp(dc, tape), inp)
        case 'l' => nxt(pc + 1, dc, bop.doLoop(dc, tape), inp)}
      case (op, Left(num)) => op match{
        case 'm' => nxt(pc + 1, dc + num, tape, inp)
        case '/' => nxt(pc + 1, scan(tape, num, dc), tape, inp)
        case '[' => nxt(if(tape(dc) == 0) num else pc + 1, dc, tape, inp)
        case ']' => nxt(if(tape(dc) != 0) num else pc + 1, dc, tape, inp)
        case ',' => nxt(pc + 1, dc, tape.set(dc, inp.head.toInt), inp.tail)
        case '.' => Some((tape(dc).toChar, (pc + 1, dc, tape, inp)))
        case 'e' => None}}
    
    inputs => LazyList.unfold((0: Int, 0: Int, MemTape(Vector.fill(init)(0), dyn, 0), inputs)){case (pc, dc, tape, inp) => nxt(pc, dc, tape, inp)}}
}
