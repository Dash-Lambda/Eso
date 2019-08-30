package brainfuck

import common.{Config, Interpreter}

import scala.annotation.tailrec
import scala.util.Try

object BFBase extends Interpreter{
  val name: String = "BFBase"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    Try{(config.num("init"), config.bool("dyn"))}
      .map{case (init, dyn) => bfRun(init, dyn, progRaw.filter("[]<>+-,.".contains(_)).toVector :+ 'e')}
  }
  
  def bfRun(init: Int, dyn: Boolean, prog: Vector[Char]): Seq[Char] => LazyList[Char] = {
    @tailrec
    def jump(i: Int, c: Int, stp: Boolean): Int = prog(i) match{
      case '[' if stp => jump(i + 1, c + 1, stp)
      case ']' if stp && c > 0 => jump(i + 1, c - 1, stp)
      case ']' if stp && c == 0 => i + 1
      case ']' if !stp => jump(i - 1, c + 1, stp)
      case '[' if !stp && c > 0 => jump(i - 1, c - 1, stp)
      case '[' if !stp && c == 0 => i + 1
      case _ =>
        if(stp) jump(i + 1, c, stp)
        else jump(i - 1, c, stp)
    }
    
    @tailrec
    def nxt(pc: Int, dc: Int, tape: Vector[Int], inp: Seq[Char]): Option[(Char, (Int, Int, Vector[Int], Seq[Char]))] = prog(pc) match{
      case '>' => nxt(pc + 1, dc + 1, if(dyn) tape.padTo(dc + 2, 0) else tape, inp)
      case '<' => nxt(pc + 1, dc - 1, tape, inp)
      case '+' => nxt(pc + 1, dc, tape.updated(dc, tape(dc) + 1), inp)
      case '-' => nxt(pc + 1, dc, tape.updated(dc, tape(dc) - 1), inp)
      case '[' if tape(dc) == 0 => nxt(jump(pc, -1, stp = true), dc, tape, inp)
      case ']' if tape(dc) != 0 => nxt(jump(pc, -1, stp = false), dc, tape, inp)
      case ',' => nxt(pc + 1, dc, tape.updated(dc, inp.head.toInt), inp.tail)
      case '.' => Some((tape(dc).toChar, (pc + 1, dc, tape, inp)))
      case 'e' => None
      case _ => nxt(pc + 1, dc, tape, inp)
    }
    
    inputs => LazyList.unfold((0: Int, 0: Int, Vector.fill(init)(0), inputs)){case (pc, dc, tape, inp) => nxt(pc, dc, tape, inp)}
  }
}
