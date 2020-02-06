package volatile

import common.{Config, OrderedParser, Interpreter, OrderedPartialParser}
import spire.math.SafeLong

import scala.annotation.tailrec
import scala.util.{Success, Try}

object Volatile extends Interpreter{
  val name: String = "Volatile"
  
  val volParser: OrderedParser[Vector[Char], VOP] = {
    OrderedPartialParser.simple{
      case c +: cs =>
        val op = c match{
          case '~' => PUSH
          case '+' => ADD
          case '-' => SUBT
          case '*' => MULT
          case '/' => DIV
          case ':' => DUP
          case '.' => OUT
          case '(' => LSTART(-1)
          case ')' => LEND(-1)}
        (op, cs)}}
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    val prog = parse(progRaw)
    def dop(stk: Vector[SafeLong])(f: (SafeLong, SafeLong) => SafeLong): Vector[SafeLong] = stk match{
      case y +: x +: ns => f(x, y) +: ns}
    @tailrec
    def vdo(i: Int, stk: Vector[SafeLong], rands: LazyList[Int]): Option[(Char, (Int, Vector[SafeLong], LazyList[Int]))] = prog.lift(i) match{
      case Some(op) => op match{
        case PUSH => rands match{
          case r +: rs => vdo(i + 1, SafeLong(r) +: stk, rs)}
        case ADD => vdo(i + 1, dop(stk)(_+_), rands)
        case SUBT => vdo(i + 1, dop(stk)(_-_), rands)
        case MULT => vdo(i + 1, dop(stk)(_*_), rands)
        case DIV => vdo(i + 1, dop(stk)(_/_), rands)
        case DUP => vdo(i + 1, stk.head +: stk, rands)
        case LSTART(n) =>
          if(stk.head == 0) vdo(n, stk, rands)
          else vdo(i + 1, stk, rands)
        case LEND(n) =>
          if(stk.head == 0) vdo(i + 1, stk, rands)
          else vdo(n, stk, rands)
        case OUT => Some((stk.head.toChar, (i + 1, stk, rands)))}
      case _ => None}
    Success(_ => LazyList.unfold((0, Vector[SafeLong](), config.rands)){case (i, stk, rs) => vdo(i, stk, rs)})}
  
  def parse(progRaw: String): Vector[VOP] = {
    @tailrec
    def sdo(src: LazyList[(VOP, Int)], ac: Vector[VOP] = Vector(), stk: Vector[Int] = Vector()): Vector[VOP] = src match{
      case (op, i) +: ops => op match{
        case LSTART(_) => sdo(ops, ac :+ op, i +: stk)
        case LEND(_) => stk match{
          case n +: ns => sdo(ops, ac.updated(n, LSTART(i + 1)) :+ LEND(n + 1), ns)}
        case _ => sdo(ops, ac :+ op, stk)}
      case _ => ac}
    sdo(volParser.parseAllValuesLazy(filterChars(progRaw, "~+-*/:.()").toVector).zipWithIndex)}
  
  trait VOP
  object PUSH extends VOP
  object ADD extends VOP
  object SUBT extends VOP
  object MULT extends VOP
  object DIV extends VOP
  object DUP extends VOP
  object OUT extends VOP
  case class LSTART(i: Int) extends VOP
  case class LEND(i: Int) extends VOP
}
