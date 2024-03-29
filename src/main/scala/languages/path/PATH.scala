package languages.path

import common.{Config, Interpreter, Matrix, MemTape, Vec2D}

import scala.annotation.tailrec
import scala.util.Try

object PATH extends Interpreter{
  val name: String = "PATH"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    Try{pathRun(Matrix.fromString(progRaw), config.num("init"), config.bool("dyn"))}}
  
  def pathRun(prog: Matrix[Char], initTapeSize: Int, dyn: Boolean): Seq[Char] => LazyList[Char] = {
    @tailrec
    def rdo(ip: POB): Option[(Char, POB)] = ip(prog) match{
      case PEND => None
      case POUT(c, nxt) => Some((c, nxt))
      case pip: PIP => rdo(pip)}
    val initIP = prog.coordOf('$') match{
      case Some(v) => v
      case None => Vec2D(0, 0)}
    inputs => LazyList.unfold(PIP(initIP, Vec2D(1, 0), MemTape(Vector.fill(initTapeSize)(0), dyn, 0), 0, inputs): POB)(rdo)}
  
  trait POB{
    def apply(prog: Matrix[Char]): POB
  }
  object PEND extends POB{
    def apply(prog: Matrix[Char]): POB = PEND
  }
  case class POUT(c: Char, nxt: POB) extends POB{
    def apply(prog: Matrix[Char]): POB = nxt
  }
  case class PIP(ip: Vec2D[Int], dt: Vec2D[Int], tape: MemTape[Int], tp: Int, inp: Seq[Char]) extends POB{
    def stp(ndt: Vec2D[Int]): PIP = PIP(ip + ndt, ndt, tape, tp, inp)
    
    def apply(prog: Matrix[Char]): POB = prog.get(ip) match{
      case None => PEND
      case Some(c) => c match{
        case '#' => PEND
        case '+' => PIP(ip + dt, dt, tape.inc(tp, 1), tp, inp)
        case '-' => PIP(ip + dt, dt, tape.inc(tp, -1), tp, inp)
        case '}' => PIP(ip + dt, dt, tape, tp + 1, inp)
        case '{' => PIP(ip + dt, dt, tape, tp - 1, inp)
        case ',' => PIP(ip + dt, dt, tape.set(tp, inp.head.toInt), tp, inp.tail)
        case '.' => POUT(tape(tp).toChar, stp(dt))
        case '!' => PIP(ip + dt*2, dt, tape, tp, inp)
        case '^' => stp(if(tape(tp) == 0) Vec2D(0, -1) else dt)
        case '<' => stp(if(tape(tp) == 0) Vec2D(-1, 0) else dt)
        case '>' => stp(if(tape(tp) == 0) Vec2D(1, 0) else dt)
        case 'v' => stp(if(tape(tp) == 0) Vec2D(0, 1) else dt)
        case '/' => stp(dt match{case Vec2D(a, b) => Vec2D(-b, -a)})
        case '\\' => stp(dt match{case Vec2D(a, b) => Vec2D(b, a)})
        case _ => stp(dt)}}}
}
