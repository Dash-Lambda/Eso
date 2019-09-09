package emmental

import common.EsoObj

import scala.annotation.tailrec
import scala.collection.immutable

case class EMState(prog: Vector[Char], inp: Seq[Char], stack: Vector[Int], queue: Vector[Int], ops: immutable.HashMap[Char, Vector[Char]]) extends EsoObj {
  def doOp(): Either[EMState, Option[(Char, EMState)]] = prog match {
    case c +: cs => ops.get(c) match{
      case Some(r) => Left(EMState(r ++ cs, inp, stack, queue, ops))
      case None => c match {
        case '.' => Right(Some(popChar))
        case '#' => Left(push(0))
        case '+' => Left(add)
        case '-' => Left(subt)
        case '~' => Left(log2)
        case ',' => Left(input)
        case '^' => Left(dupQueue)
        case 'v' => Left(pushQueue)
        case ':' => Left(dup)
        case '!' => Left(redefine)
        case '?' => Left(eval)
        case ';' => Left(push(';'))
        case _ if c.isDigit => Left(doMath(c.asDigit))
        case _ => Left(EMState(cs, inp, stack, queue, ops))
      }
    }
    case _ => Right(None)
  }
  
  def redefine: EMState = {
    val tag = stack.head.toChar
    val (tok, tl) = stack.tail.splitAt(stack.tail.indexOf(';'))
    val cmd = tok.reverse.flatMap{n =>
      ops.get(n.toChar) match{
        case Some(r) => r
        case None => Seq(n.toChar)
      }
    }
    
    EMState(prog.tail, inp, tl.tail, queue, ops + ((tag, cmd)))
  }
  
  def input: EMState = EMState(prog.tail, inp.tail, inp.head.toInt +: stack, queue, ops)
  def eval: EMState = EMState(stack.head.toChar +: prog.tail, inp, stack.tail, queue, ops)
  def push(n: Int): EMState = EMState(prog.tail, inp, n +: stack, queue, ops)
  def popChar: (Char, EMState) = (stack.head.toChar, EMState(prog.tail, inp, stack.tail, queue, ops))
  def dup: EMState = EMState(prog.tail, inp, stack.head +: stack, queue, ops)
  def dupQueue: EMState = EMState(prog.tail, inp, stack, queue :+ stack.head, ops)
  def pushQueue: EMState = EMState(prog.tail, inp, queue.head +: stack, queue.tail, ops)
  
  def doMath(n: Int): EMState = EMState(prog.tail, inp, (((stack.head*10) + n)%256) +: stack.tail, queue, ops)
  def add: EMState = stack match{case a +: b +: ns => EMState(prog.tail, inp, (a + b)%256 +: ns, queue, ops)}
  def subt: EMState = stack match{case a +: b +: ns => EMState(prog.tail, inp, (256 + b - a)%256 +: ns, queue, ops)}
  def log2: EMState = {
    if(stack.head == 0) EMState(prog.tail, inp, 8 +: stack.tail, queue, ops)
    else EMState(prog.tail, inp, EMState.dlog2(stack.head)%256 +: stack.tail, queue, ops)
  }
}
object EMState {
  @tailrec def dlog2(n: Int, c: Int = 0): Int = if(n <= 1) c else dlog2(n/2, c + 1)
  
  def apply(progRaw: String, inp: Seq[Char]): EMState = new EMState(progRaw.toVector, inp, Vector(), Vector(), immutable.HashMap[Char, Vector[Char]]())
  def apply(prog: Vector[Char], inp: Seq[Char], stack: Vector[Int], queue: Vector[Int], ops: immutable.HashMap[Char, Vector[Char]]): EMState = new EMState(prog, inp, stack, queue, ops)
}
