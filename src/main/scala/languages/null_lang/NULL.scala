package languages.null_lang

import common.{Config, Interpreter, PrimeNumTools}
import spire.math.SafeLong
import spire.implicits._

import scala.annotation.tailrec
import scala.util.Try

object NULL extends Interpreter{
  val name: String = "NULL"
  val primes: LazyList[SafeLong] = PrimeNumTools.birdPrimes.to(LazyList)
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = Try{SafeLong(BigInt(progRaw.filter(_.isDigit)))} map nullRun
  
  def nullRun(initProg: SafeLong): Seq[Char] => LazyList[Char] = {
    @tailrec
    def ndo(state: State): Option[(Char, State)] = state.next match{
      case HaltRet => None
      case OutRet(c, nxt) => Some((c, nxt))
      case ContRet(nxt) => ndo(nxt)}
    inputs => LazyList.unfold(State(Vector(), Vector(), Vector(), initProg, 1, inputs))(ndo)}
  
  trait NRet
  object HaltRet extends NRet
  case class OutRet(c: Char, nxt: State) extends NRet
  case class ContRet(nxt: State) extends NRet
  case class State(q0: Vector[Short], q1: Vector[Short], q2: Vector[Short], x: SafeLong, y: SafeLong, inp: Seq[Char]){
    def curHead: Short = q0.headOption.getOrElse(0)
    def ubyte(n: SafeLong): Short = ((n%256 + 256)%256).toShort
    def getOp(x0: SafeLong, y0: SafeLong): Option[(Int, SafeLong, SafeLong)] = {
      if(x <= 1) None
      else (primes zip LazyList.continually(0 to 13).flatten)
        .collectFirst{
          case (p, i) if x%p == 0 => (i, x/p, y*p)}}
    def next: NRet = getOp(x, y) match{
      case None => HaltRet
      case Some((op, nx, ny)) => op match{
        case 0 => ContRet(State(q1, q2, q0, nx, ny, inp)) //2
        case 1 => ContRet(State(q2, q0, q1, nx, ny, inp)) //3
        case 2 => OutRet(curHead.toChar, State(q0, q1, q2, nx, ny, inp)) //5
        case 3 => inp match{ //7
          case c +: cs => ContRet(State(ubyte(SafeLong(c)) +: q0.drop(1), q1, q2, nx, ny, cs))}
        case 4 => ContRet(State(q0, q1, q2, nx, (ny - curHead).max(0), inp)) //11
        case 5 => ContRet(State(q0, q1, q2, nx, ny + curHead, inp)) //13
        case 6 => ContRet(State(ubyte(ny + curHead) +: q0.drop(1), q1, q2, nx, ny, inp)) //17
        case 7 => ContRet(State(q0.drop(1), q1 :+ curHead, q2, nx, ny, inp)) //19
        case 8 => ContRet(State(q0.drop(1), q1, q2 :+ curHead, nx, ny, inp)) //23
        case 9 => ContRet(State(q0.drop(1), q1, q2, nx, ny, inp)) //29
        case 10 => ContRet(State(q0 :+ ubyte(ny), q1, q2, nx, ny, inp)) //31
        case 11 => //37
          if(curHead != 0) ContRet(State(q0, q1, q2, nx, ny, inp))
          else getOp(nx, ny) match{
            case Some((_, nx2, ny2)) => ContRet(State(q0, q1, q2, nx2, ny2, inp))
            case _ => ContRet(State(q0, q1, q2, nx, ny, inp))}
        case 12 => ContRet(State(q0, q1, q2, ny, nx, inp)) //41
        case 13 => HaltRet}} //43
  }
}