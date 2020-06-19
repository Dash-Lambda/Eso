package brainfuck

import common.{EsoObj, MemTape}

import scala.annotation.tailrec
import scala.util.Try

trait BFOp extends EsoObj
trait BlkOp extends BFOp{
  def shift: Int
  def maxShift: Int
  def apply(p: Int, dat: MemTape[Int]): MemTape[Int]
}

case class BFMove(n: Int) extends BFOp
case class BFScan(n: Int) extends BFOp
case class BFOpenLoop(i: Int) extends BFOp
case class BFCloseLoop(i: Int) extends BFOp
case class BFOut(n: Int) extends BFOp
object BFIn extends BFOp
object BFEnd extends BFOp

case class LoopOp(ops: Vector[(Int, Option[Int])]) extends BlkOp{
  def shift: Int = 0
  def maxShift: Int = ops.map(_._1).max
  def apply(p: Int, dat: MemTape[Int]): MemTape[Int] = {
    val num = dat(p)
    if(num == 0) dat
    else ops.foldLeft(dat){
      case (ac, (i, Some(n))) => ac.inc(p + i, num*n)
      case (ac, (i, None)) => ac.set(p + i, 0)}}
}
case class SingOp(ops: Vector[(Int, Option[Int])], shift: Int) extends BlkOp{
  def maxShift: Int = math.max(shift, ops.map(_._1).max)
  def isLoop: Boolean = (shift == 0) && !ops.contains((0, None)) && (ops.collect{case (0, Some(n)) => n}.sum == -1)
  def isMove: Boolean = ops.isEmpty
  def toLoop: LoopOp = LoopOp(ops)
  
  def apply(p: Int, dat: MemTape[Int]): MemTape[Int] = ops.foldLeft(dat){
    case (ac, (i, Some(n))) => ac.inc(p + i, n)
    case (ac, (i, None)) => ac.set(p + i, 0)}
}

object BFOptimize extends EsoObj{
  val blks: Vector[Char] = Vector('<', '>', '+', '-', '_')
  val opts: Vector[Char] = Vector('<', '>', '+', '-', '.')
  
  def apply(progRaw: String, comp: Boolean = false): Try[Vector[BFOp]] = Try{optFull(progRaw, comp)}
  
  def optFull(progRaw: String, comp: Boolean = false): Vector[BFOp] = {
    def contract(src: String): Vector[(Char, Int)] = {
      @tailrec
      def cdo(i: Int, ac: Vector[(Char, Int)]): Vector[(Char, Int)] = src.lift(i) match{
        case Some(c) =>
          if(opts.contains(c)){
            val i1 = src.indexWhere(_ != c, i)
            cdo(i1, ac :+ (c, i1 - i))}
          else cdo(i + 1, ac :+ (c, 0))
        case None => ac}
      cdo(0, Vector())}
    
    def collectBulk(src: Vector[(Char, Int)]): Vector[BFOp] = {
      @tailrec
      def bdo(i: Int, ac: Vector[(Int, Option[Int])], s: Int): (Vector[(Int, Option[Int])], Int, Int) = src.lift(i) match{
        case Some((c, n)) => c match{
          case '+' => bdo(i + 1, ac :+ (s, Some(n)), s)
          case '-' => bdo(i + 1, ac :+ (s, Some(-n)), s)
          case '_' => bdo(i + 1, ac :+ (s, None), s)
          case '>' => bdo(i + 1, ac, s + n)
          case '<' => bdo(i + 1, ac, s - n)
          case _ => (ac, s, i)}
        case _ => (ac, s, i)}
      
      @tailrec
      def cdo(i: Int, ac: Vector[BFOp]): Vector[BFOp] = src.lift(i) match{
        case Some((c, n)) => c match{
          case '+' | '-' | '<' | '>' | '_' => bdo(i, Vector(), 0) match{
            case (vec, s, i1) =>
              if(vec.nonEmpty) cdo(i1, ac :+ SingOp(vec, s))
              else cdo(i1, ac :+ BFMove(s))}
          case '.' => cdo(i + 1, ac :+ BFOut(n))
          case _ =>
            val bfop = c match{
              case '[' => BFOpenLoop(-1)
              case ']' => BFCloseLoop(-1)
              case ',' => BFIn
              case 'e' => BFEnd}
            cdo(i + 1, ac :+ bfop)}
        case _ => ac}
      cdo(0, Vector())}
    
    def setLoops(srcRaw: Vector[BFOp]): Vector[BFOp] = {
      @tailrec
      def ldo(ac: Vector[BFOp], src: Vector[BFOp]): Vector[BFOp] = src match{
        case BFOpenLoop(_) +: BFMove(n) +: BFCloseLoop(_) +: ops => ldo(ac :+ BFScan(n), ops)
        case BFOpenLoop(_) +: (bop: SingOp) +: BFCloseLoop(_) +: ops =>
          if(bop.isLoop) ldo(ac :+ bop.toLoop, ops)
          else ldo(ac :+ BFOpenLoop(-1), bop +: BFCloseLoop(-1) +: ops)
        case op +: ops => ldo(ac :+ op, ops)
        case _ => ac}
      ldo(Vector(), srcRaw)}

    def setSkipsComp(src: Vector[BFOp]): Vector[BFOp] = {
      @tailrec
      def sdo(ac: Vector[BFOp], stk: Vector[Int], i: Int): Vector[BFOp] = src.lift(i) match{
        case None => ac
        case Some(op) => op match{
          case BFOpenLoop(_) => sdo(ac, stk :+ i, i + 1)
          case BFCloseLoop(_) => stk match{
            case ls :+ l => sdo(ac.updated(l, BFOpenLoop(0)).updated(i, BFCloseLoop(0)), ls, i + 1)
            case _ => sdo(ac.updated(i, BFCloseLoop(1)), stk, i + 1)}
          case BFOut(_) => sdo(stk.foldLeft(ac){case (p, l) => p.updated(l, BFOpenLoop(1))}, Vector(), i + 1)
          case _ => sdo(ac, stk, i + 1)}}
      sdo(src, Vector(), 0)}
    
    def setSkips(src: Vector[BFOp]): Vector[BFOp] = {
      @tailrec
      def sdo(i: Int, stk: Vector[Int], ac: Vector[BFOp]): Vector[BFOp] = src.lift(i) match{
        case Some(op) => op match{
          case BFOpenLoop(_) => sdo(i + 1, i +: stk, ac)
          case BFCloseLoop(_) => sdo(i + 1, stk.tail, ac.updated(stk.head, BFOpenLoop(i + 1)).updated(i, BFCloseLoop(stk.head + 1)))
          case _ => sdo(i + 1, stk, ac)}
        case _ => ac}
      sdo(0, Vector(), src)}
    
    val prog1 = filterChars(progRaw, "[]<>+-,.").replaceAll("""\[[+\-]]""", "_") :+ 'e'
    val prog2 = contract(prog1)
    val prog3 = collectBulk(prog2)
    val prog4 = setLoops(prog3)
    if(comp) setSkipsComp(prog4) else setSkips(prog4)}
}
