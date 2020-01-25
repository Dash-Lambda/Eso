package whitespace

import common.{Config, Interpreter}
import WSCommon._
import spire.math.SafeLong
import spire.implicits._

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.Try

object WhiteSpace extends Interpreter{
  val name: String = "WhiteSpace"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = Try{parse(progRaw)} map{prog => wsRun(prog, getCalls(prog))}
  
  def wsRun(prog: Vector[(String, SafeLong)], calls: immutable.HashMap[SafeLong, Int]): Seq[Char] => LazyList[Char] = {
    def binOp(stack: List[SafeLong])(func: (SafeLong, SafeLong) => SafeLong): List[SafeLong] = stack match{case n1 +: n2 +: ns => func(n1, n2) +: ns}
    
    @tailrec
    def wsi(pc: Int, stack: List[SafeLong], heap: immutable.HashMap[SafeLong, SafeLong], callStack: List[Int], inp: Seq[Char]): Option[(String, (Int, List[SafeLong], immutable.HashMap[SafeLong, SafeLong], List[Int], Seq[Char]))] = {
      val (op, num) = prog(pc)
      op match{
        case "push" => wsi(pc + 1, num +: stack, heap, callStack, inp)
        case "dup" => wsi(pc + 1, stack.head +: stack, heap, callStack, inp)
        case "swap" => stack match{case n1 +: n2 +: ns => wsi(pc + 1, n2 +: n1 +: ns, heap, callStack, inp)}
        case "discard" => wsi(pc + 1, stack.tail, heap, callStack, inp)
        case "add" => wsi(pc + 1, binOp(stack)(_+_), heap, callStack, inp)
        case "subt" => wsi(pc + 1, binOp(stack)(_-_), heap, callStack, inp)
        case "mult" => wsi(pc + 1, binOp(stack)(_*_), heap, callStack, inp)
        case "intDiv" => wsi(pc + 1, binOp(stack)(_/_), heap, callStack, inp)
        case "mod" => wsi(pc + 1, binOp(stack)(_%_), heap, callStack, inp)
        case "store" => stack match{case n +: addr +: ns => wsi(pc + 1, ns, heap + ((addr, n)), callStack, inp)}
        case "get" => wsi(pc + 1, heap(stack.head) +: stack.tail, heap, callStack, inp)
        case "label" => wsi(pc + 1, stack, heap, callStack, inp)
        case "call" => wsi(calls(num), stack, heap, (pc + 1) +: callStack, inp)
        case "jump" => wsi(calls(num), stack, heap, callStack, inp)
        case "jumpZero" =>
          if(stack.head == 0) wsi(calls(num), stack.tail, heap, callStack, inp)
          else wsi(pc + 1, stack.tail, heap, callStack, inp)
        case "jumpNeg" =>
          if(stack.head < 0) wsi(calls(num), stack.tail, heap, callStack, inp)
          else wsi(pc + 1, stack.tail, heap, callStack, inp)
        case "return" => wsi(callStack.head, stack, heap, callStack.tail, inp)
        case "readChar" => wsi(pc + 1, SafeLong(inp.head.toInt) +: stack, heap, callStack, inp.tail)
        case "readNum" => wsi(pc + 1, SafeLong(BigInt(inp.takeWhile(_.isDigit).mkString)) +: stack, heap, callStack, inp.dropWhile(_.isDigit))
        case "outChar" => Some((stack.head.toChar.toString, (pc + 1, stack.tail, heap, callStack, inp)))
        case "outNum" => Some((stack.head.toString, (pc + 1, stack.tail, heap, callStack, inp)))
        case "endProg" => None}}
    inputs => LazyList.unfold((0: Int, List[SafeLong](), immutable.HashMap[SafeLong, SafeLong](), List[Int](), inputs)){
      case (pc, stack, heap, callStack, inp) => wsi(pc, stack, heap, callStack, inp)}.flatten}
}
