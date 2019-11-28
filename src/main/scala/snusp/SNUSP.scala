package snusp

import common.{Config, Interpreter, Matrix, Vec2D}

import scala.annotation.tailrec
import scala.util.{Random, Try}

object SNUSP extends Interpreter{
  val name: String = "SNUSP"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    Try{Matrix.fromString(progRaw)} map{prog =>
      snuRun(prog, config.num("init"), config.rand)
    }
  }
  
  def snuRun(prog: Matrix[Char], tapeSize: Int, rand: Random): Seq[Char] => LazyList[Char] = {
    @tailrec
    def sdo(state: State, ips: Vector[SNOB]): Option[(Char, (State, Vector[SNOB]))] = ips match{
      case ip +: tl => ip match{
        case SNOP => sdo(state, tl)
        case SNOUT(c, nxt) => Some((c, (state, tl :+ nxt)))
        case SNLIT(ip1, ip2) => sdo(state, ip2 +: (tl :+ ip1))
        case snip: SNIP => snip(prog, state, rand) match{
          case (state, nxt) => sdo(state, tl :+ nxt)
        }
      }
      case _ => None
    }
    
    val initIP = prog.coordOf('$') match{
      case Some(v) => SNIP(v, Vec2D(1, 0), Vec2D(0, 0), Vector())
      case None => SNIP(Vec2D(0, 0), Vec2D(1, 0), Vec2D(0, 0), Vector())
    }
    
    inputs => LazyList.unfold((State(Matrix.fill(tapeSize, 1)(0), inputs, dyn = true): State, Vector(initIP): Vector[SNOB])){
      case (state, sip) => sdo(state, sip)
    }
  }
  
  case class State(data: Matrix[Int], inp: Seq[Char], dyn: Boolean){
    def padded(v: Vec2D[Int]): Matrix[Int] = data.padToAbs(v + Vec2D(1, 1), 0)
    
    def apply(v: Vec2D[Int]): Int = {
      if(dyn && !data.isDefinedAt(v)) 0
      else data(v)
    }
    
    def set(v: Vec2D[Int], n: Int): State = {
      if(dyn && !data.isDefinedAt(v)) State(padded(v).updated(v, n), inp, dyn)
      else State(data.updated(v, n), inp, dyn)
    }
    
    def inc(v: Vec2D[Int], n: Int): State = {
      if(dyn && !data.isDefinedAt(v)){
        val ndat = padded(v)
        State(ndat.updated(v, ndat(v) + n), inp, dyn)
      }
      else State(data.updated(v, data(v) + n), inp, dyn)
    }
    
    def read(v: Vec2D[Int]): State = {
      if(dyn && !data.isDefinedAt(v)) State(padded(v).updated(v, inp.head), inp.tail, dyn)
      else State(data.updated(v, inp.head), inp.tail, dyn)
    }
    
    def out(v: Vec2D[Int]): Char = {
      if(dyn && !data.isDefinedAt(v)) 0.toChar
      else data(v).toChar
    }
  }
  
  trait SNOB
  object SNOP extends SNOB{
    override def toString: String = "SNOP"
  }
  case class SNOUT(c: Char, nxt: SNOB) extends SNOB
  case class SNLIT(ip1: SNOB, ip2: SNOB) extends SNOB
  case class SNIP(ip: Vec2D[Int], dt: Vec2D[Int], dp: Vec2D[Int], calls: Vector[(Vec2D[Int], Vec2D[Int])]) extends SNOB{
    def stp(ndt: Vec2D[Int]): SNIP = SNIP(ip + ndt, ndt, dp, calls)
    def LURDdt: Vec2D[Int] = dt match{
      case Vec2D(-1, 0) => Vec2D(0, -1)
      case Vec2D(0, -1) => Vec2D(-1, 0)
      case Vec2D(1, 0) => Vec2D(0, 1)
      case Vec2D(0, 1) => Vec2D(1, 0)
    }
    def RULDdt: Vec2D[Int] = dt match{
      case Vec2D(1, 0) => Vec2D(0, -1)
      case Vec2D(0, -1) => Vec2D(1, 0)
      case Vec2D(-1, 0) => Vec2D(0, 1)
      case Vec2D(0, 1) => Vec2D(-1, 0)
    }
    
    def apply(prog: Matrix[Char], state: State, rand: Random): (State, SNOB) = prog.get(ip) match{
      case None => (state, SNOP)
      case Some(op) => op match{
        case '>' => (state, SNIP(ip + dt, dt, dp + Vec2D(1, 0), calls))
        case '<' => (state, SNIP(ip + dt, dt, dp + Vec2D(-1, 0), calls))
        case ':' => (state, SNIP(ip + dt, dt, dp + Vec2D(0, -1), calls))
        case ';' => (state, SNIP(ip + dt, dt, dp + Vec2D(0, 1), calls))
        case '+' => (state.inc(dp, 1), stp(dt))
        case '-' => (state.inc(dp, -1), stp(dt))
        case '%' => (state.set(dp, rand.nextInt()), stp(dt))
        case ',' => (state.read(dp), stp(dt))
        case '.' => (state, SNOUT(state.out(dp), stp(dt)))
        case '\\' => (state, stp(LURDdt))
        case '/' => (state, stp(RULDdt))
        case '!' => (state, SNIP(ip + dt*2, dt, dp, calls))
        case '?' =>
          if(state(dp) == 0) (state, SNIP(ip + dt*2, dt, dp, calls))
          else (state, stp(dt))
        case '@' => (state, SNIP(ip + dt, dt, dp, (ip, dt) +: calls))
        case '#' => calls match{
          case (nip, ndt) +: cs => (state, SNIP(nip + ndt*2, ndt, dp, cs))
          case _ => (state, SNOP)
        }
        case '&' => (state, SNLIT(SNIP(ip + dt*2, dt, dp, calls), stp(dt)))
        case _ => (state, stp(dt))
      }
    }
  }
}
