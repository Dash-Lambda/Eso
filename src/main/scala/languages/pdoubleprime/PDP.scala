package languages.pdoubleprime

import common.{Config, Interpreter, MemTape}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.{Success, Try}

object PDP extends Interpreter{
  val name: String = "P''"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    Try{(config.num("init"), config.bool("fPtr"), config.bool("sHead"), config.bool("pNull"))} flatMap{
      case (initSize, fPtr, sHead, pNull) => Try{condition(progRaw)} map{
        case (alpha, init, prog, jMap) => pdi(initSize, fPtr, sHead, pNull)(alpha, init, prog, jMap)}}}
  
  def pdi(initSize: Int, fromPtr: Boolean, startHead: Boolean, printNull: Boolean)(alpha: Vector[Char], init: Vector[Char], prog: Vector[Char], jMap: immutable.HashMap[Int, Int]): Seq[Char] => LazyList[Char] = {
    @tailrec
    def nxt(pc: Int, tc: Int, tape: MemTape[Int]): LazyList[Char] = prog(pc) match{
      case '(' => nxt(if(tape(tc) == 0) jMap(pc) + 1 else pc + 1, tc, tape)
      case ')' => nxt(if(tape(tc) != 0) jMap(pc) + 1 else pc + 1, tc, tape)
      case 'R' => nxt(pc + 1, tc + 1, tape)
      case 'A' =>
        if(tc > 0) nxt(pc + 1, tc - 1, tape.alter(tc, n => (n + 1)%alpha.size))
        else nxt(pc + 1, tc, 0 +: tape.alter(tc, n => (n + 1)%alpha.size))
      case 'e' =>
        val trunc = if(fromPtr) tape.drop(tc) else tape
        (if(printNull) trunc.to(LazyList) else trunc.to(LazyList).filter(_ > 0)).map(alpha(_))}
    val initTape = MemTape(init.map(alpha.indexOf(_)).reverse.padTo(initSize, 0).reverse, dyn=false, 0)
    _ => nxt(0, if(startHead) math.max(0, initSize - init.size) else initSize - 1, initTape)}
  
  def condition(progRaw: String): (Vector[Char], Vector[Char], Vector[Char], immutable.HashMap[Int, Int]) = {
    val lines = progRaw.linesIterator.to(LazyList)
    val alpha = lines.head.toVector
    val init = lines.tail.head.toVector
    val prog = filterChars(
      lines
        .tail.tail
        .mkString
        .replace("L", "r'A")
        .replace("r'", "r"*(alpha.length - 1))
        .replace("r", "AR"),
      "()RA")
      .toVector
    lazy val jMap = mdo(Vector[(Int, Int)](), List[Int](), 0)
    
    @tailrec
    def mdo(ac: Vector[(Int, Int)], stack: List[Int], i: Int): immutable.HashMap[Int, Int] = Try{prog(i)} match{
      case Success(c) => c match{
        case '(' => mdo(ac, i +: stack, i + 1)
        case ')' => mdo(ac :+ (i, stack.head) :+ (stack.head, i), stack.tail, i + 1)
        case _ => mdo(ac, stack, i + 1)}
      case _ => mkMap(ac)}
    (alpha, init, prog :+ 'e', jMap)}
}
