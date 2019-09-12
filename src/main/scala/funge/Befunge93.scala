package funge

import common.{Config, Interpreter}

import scala.annotation.tailrec
import scala.util.{Random, Try}

object Befunge93 extends Interpreter{
  val name: String = "Befunge-93"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = Try{bfgRun(progRaw)}
  
  def bfgRun(progRaw: String): Seq[Char] => LazyList[Char] = {
    val rand = new Random()
    val bfProg = BFGProg(progRaw, 80, 25)
    
    def chomp(inp: Seq[Char]): (String, Seq[Char]) = {
      val (hd, tl) = inp.splitAt(inp.indexOf('\n'))
      (hd.mkString, tl.tail)
    }
    
    def op1(stk: LazyList[Long], f: Long => Seq[Long]): LazyList[Long] = f(stk.head) ++: stk.tail
    def op2(stk: LazyList[Long], f: (Long, Long) => Seq[Long]): LazyList[Long] = stk match{
      case a +: b +: ns => f(a, b) ++: ns
    }
    def mop1(stk: LazyList[Long], f: Long => Long): LazyList[Long] = f(stk.head) #:: stk.tail
    def mop(stk: LazyList[Long], f: (Long, Long) => Long): LazyList[Long] = stk match{
      case a +: b +: ns => f(a, b) +: ns
    }
    
    @tailrec
    def bfi(px: Int, py: Int, dx: Int, dy: Int, bs: Boolean, stk: LazyList[Long], inp: Seq[Char], prog: BFGProg): Option[(String, (Int, Int, Int, Int, Boolean, LazyList[Long], Seq[Char], BFGProg))] = {
      prog.get(px, py) match{
        case Some(c) if bs =>
          if(c == '"') bfi(px + dx, py + dy, dx, dy, false, stk, inp, prog)
          else bfi(px + dx, py + dy, dx, dy, bs, c.toLong #:: stk, inp, prog)
        case Some(c) => c match{
          case '>' => bfi(px + 1, py, 1, 0, bs, stk, inp, prog)
          case '<' => bfi(px - 1, py, -1, 0, bs, stk, inp, prog)
          case '^' => bfi(px, py - 1, 0, -1, bs, stk, inp, prog)
          case 'v' => bfi(px, py + 1, 0, 1, bs, stk, inp, prog)
          case '?' => rand.nextInt(4) match {
            case 0 => bfi(px + 1, py, 1, 0, bs, stk, inp, prog)
            case 1 => bfi(px - 1, py, -1, 0, bs, stk, inp, prog)
            case 2 => bfi(px, py + 1, 0, 1, bs, stk, inp, prog)
            case 3 => bfi(px, py - 1, 0, -1, bs, stk, inp, prog)
          }
          case '_' =>
            if(stk.head == 0) bfi(px + 1, py, 1, 0, bs, stk.tail, inp, prog)
            else bfi(px - 1, py, -1, 0, bs, stk.tail,inp, prog)
          case '|' =>
            if(stk.head == 0) bfi(px, py + 1, 0, 1, bs, stk.tail, inp, prog)
            else bfi(px, py - 1, 0, -1, bs, stk.tail, inp, prog)
          case '"' => bfi(px + dx, py + dy, dx, dy, true, stk, inp, prog)
          case '&' =>
            val (tok, rem) = chomp(inp)
            bfi(px + dx, py + dy, dx, dy, bs, tok.toLong #:: stk, rem, prog)
          case '~' => bfi(px + dx, py + dy, dx, dy, bs, inp.head.toLong #:: stk, inp.tail, prog)
          case '.' => Some((stk.head.toString, (px + dx, py + dy, dx, dy, bs, stk.tail, inp, prog)))
          case ',' => Some((stk.head.toChar.toString, (px + dx, py + dy, dx, dy, bs, stk.tail, inp, prog)))
          case '#' => bfi(px + 2*dx, py + 2*dy, dx, dy, bs, stk, inp, prog)
          case ':' => bfi(px + dx, py + dy, dx, dy, bs, op1(stk, n => Seq(n, n)), inp, prog)
          case '$' => bfi(px + dx, py + dy, dx, dy, bs, stk.tail, inp, prog)
          case '\\' => bfi(px + dx, py + dy, dx, dy, bs, op2(stk, (a, b) => Seq(b, a)), inp, prog)
          case '`' => bfi(px + dx, py + dy, dx, dy, bs, mop(stk, (a, b) => if(a < b) 1L else 0L), inp, prog)
          case 'g' => bfi(px + dx, py + dy, dx, dy, bs, mop(stk, (y, x) => prog(x, y).toLong), inp, prog)
          case 'p' => stk match{
            case y +: x +: rep +: rem => bfi(px + dx, py + dy, dx, dy, bs, rem, inp, prog.updated(x.toInt, y.toInt, rep.toChar))
          }
          case '+' => bfi(px + dx, py + dy, dx, dy, bs, mop(stk, (a, b) => a + b), inp, prog)
          case '-' => bfi(px + dx, py + dy, dx, dy, bs, mop(stk, (a, b) => b - a), inp, prog)
          case '*' => bfi(px + dx, py + dy, dx, dy, bs, mop(stk, (a, b) => a * b), inp, prog)
          case '/' => bfi(px + dx, py + dy, dx, dy, bs, mop(stk, (a, b) => b / a), inp, prog)
          case '%' => bfi(px + dx, py + dy, dx, dy, bs, mop(stk, (a, b) => b % a), inp, prog)
          case '!' => bfi(px + dx, py + dy, dx, dy, bs, mop1(stk, n => if(n == 0) 1L else 0L), inp, prog)
          case '@' => None
          case _ =>
            if(c.isDigit) bfi(px + dx, py + dy, dx, dy, bs, c.asDigit #:: stk, inp, prog)
            else bfi(px + dx, py + dy, dx, dy, bs, stk, inp, prog)
        }
        case None =>
          val nx = (px + prog.xdim)%prog.xdim
          val ny = (py + prog.ydim)%prog.ydim
          bfi(nx, ny, dx, dy, bs, stk, inp, prog)
      }
    }
    
    inputs => LazyList.unfold((0: Int, 0: Int, 1: Int, 0: Int, false: Boolean, LazyList.continually(0L), inputs, bfProg)) {
      case (px, py, dx, dy, bs, stk, inp, prog) => bfi(px, py, dx, dy, bs, stk, inp, prog)
    }.flatten
  }
}
