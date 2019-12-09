package brainfuck

import common.{Config, Interpreter, MemTape}

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
    def scrub(pc: Int, c: Int = 0): Int = prog(pc) match{
      case ']' =>
        if(c == 1) pc + 1
        else scrub(pc + 1, c - 1)
      case '[' => scrub(pc + 1, c + 1)
      case _ => scrub(pc + 1, c)
    }
    
    @tailrec
    def step(pc: Int, dc: Int, tape: MemTape[Int], inp: Seq[Char], loops: Vector[Int]): Option[(Char, (Int, Int, MemTape[Int], Seq[Char], Vector[Int]))] = prog(pc) match{
      case '>' => step(pc + 1, dc + 1, tape, inp, loops)
      case '<' => step(pc + 1, dc - 1, tape, inp, loops)
      case '+' => step(pc + 1, dc, tape.inc(dc, 1), inp, loops)
      case '-' => step(pc + 1, dc, tape.inc(dc, -1), inp, loops)
      case '[' =>
        if(tape(dc) == 0) step(scrub(pc), dc, tape, inp, loops)
        else step(pc + 1, dc, tape, inp, (pc + 1) +: loops)
      case ']' =>
        if(tape(dc) != 0) step(loops.head, dc, tape, inp, loops)
        else step(pc + 1, dc, tape, inp, loops.tail)
      case ',' => step(pc + 1, dc, tape.set(dc, inp.head.toInt), inp.tail, loops)
      case '.' => Some((tape(dc).toChar, (pc + 1, dc, tape, inp, loops)))
      case 'e' => None
    }
    
    inputs => LazyList.unfold((0: Int, 0: Int, MemTape(Vector.fill(init)(0), dyn, 0), inputs, Vector[Int]())){case (pc, dc, tape, inp, loops) => step(pc, dc, tape, inp, loops)}
  }
}
