package brainfuck

import common.{EsoObj, MemTape}

import scala.annotation.tailrec
import scala.util.Try

case class BlkOp(ops: Vector[(Int, Option[Int])], shift: Int) extends EsoObj{
  def maxShift: Int = math.max(shift, ops.map(_._1).max)
  def isLoop: Boolean = (shift == 0) && !ops.contains((0, None)) && (ops.collect{case (0, Some(n)) => n}.sum == -1)
  def isMove: Boolean = ops.isEmpty
  
  def doOp(p: Int, dat: MemTape[Int]): MemTape[Int] = ops.foldLeft(dat){
    case (ac, (i, Some(n))) => ac.inc(p + i, n)
    case (ac, (i, None)) => ac.set(p + i, 0)}
  def doLoop(p: Int, dat: MemTape[Int]): MemTape[Int] = {
    val num = dat(p)
    if(num == 0) dat
    else ops.foldLeft(dat){
      case (ac, (i, Some(n))) => ac.inc(p + i, num*n)
      case (ac, (i, None)) => ac.set(p + i, 0)}}
  
  override def toString: String = s"{$shift; ${ops.map{case (i, arg) => s"$i -> ${arg match{case Some(n) => n.toString; case None => "_"}}"}.mkString(", ")}}"
}

object BFOptimize extends EsoObj{
  val blks: Vector[Char] = Vector('<', '>', '+', '-', '_')
  val opts: Vector[Char] = Vector('<', '>', '+', '-')
  
  def apply(progRaw: String): Try[Vector[(Char, Either[Int, BlkOp])]] = Try{optFull(progRaw)}
  
  def optFull(progRaw: String): Vector[(Char, Either[Int, BlkOp])] = {
    def contract(src: String): Vector[(Char, Int)] = {
      @tailrec
      def cdo(i: Int, ac: Vector[(Char, Int)]): Vector[(Char, Int)] = src.lift(i) match{
        case Some(c) => c match{
          case '>' | '<' | '+' | '-' =>
            val i1 = src.indexWhere(_ != c, i)
            cdo(i1, ac :+ (c, i1 - i))
          case _ => cdo(i + 1, ac :+ (c, 0))}
        case None => ac}
      cdo(0, Vector())}
    
    def collectBulk(src: Vector[(Char, Int)]): Vector[(Char, Either[Int, BlkOp])] = {
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
      def cdo(i: Int, ac: Vector[(Char, Either[Int, BlkOp])]): Vector[(Char, Either[Int, BlkOp])] = src.lift(i) match{
        case Some((c, n)) => c match{
          case '+' | '-' | '<' | '>' | '_' => bdo(i, Vector(), 0) match{
            case (vec, s, i1) =>
              if(vec.nonEmpty) cdo(i1, ac :+ ('u', Right(BlkOp(vec, s))))
              else cdo(i1, ac :+ ('m', Left(s)))}
          case _ => cdo(i + 1, ac :+ (c, Left(n)))}
        case _ => ac}
      cdo(0, Vector())}
    
    def setLoops(srcRaw: Vector[(Char, Either[Int, BlkOp])]): Vector[(Char, Either[Int, BlkOp])] = {
      @tailrec
      def ldo(ac: Vector[(Char, Either[Int, BlkOp])], src: Vector[(Char, Either[Int, BlkOp])]): Vector[(Char, Either[Int, BlkOp])] = src match{
        case ('[', _) +: ('m', arg) +: (']', _) +: ops => ldo(ac :+ ('/', arg), ops)
        case ('[', _) +: ('u', Right(bop)) +: (']', _) +: ops =>
          if(bop.isLoop) ldo(ac :+ ('l', Right(bop)), ops)
          else ldo(ac :+ ('[', Left(0)), ('u', Right(bop)) +: (']', Left(0)) +: ops)
        case op +: ops => ldo(ac :+ op, ops)
        case _ => ac}
      ldo(Vector(), srcRaw)}
    
    def setSkips(src: Vector[(Char, Either[Int, BlkOp])]): Vector[(Char, Either[Int, BlkOp])] = {
      @tailrec
      def sdo(i: Int, stk: Vector[Int], ac: Vector[(Char, Either[Int, BlkOp])]): Vector[(Char, Either[Int, BlkOp])] = src.lift(i) match{
        case Some((c, _)) => c match{
          case '[' => sdo(i + 1, i +: stk, ac)
          case ']' => sdo(i + 1, stk.tail, ac.updated(stk.head, ('[', Left(i + 1))).updated(i, (']', Left(stk.head + 1))))
          case _ => sdo(i + 1, stk, ac)}
        case _ => ac}
      sdo(0, Vector(), src)}
    
    val prog1 = progRaw.filter("><][+-,.".contains(_)).replaceAll("""\[[+\-]\]""", "_") :+ 'e'
    val prog2 = contract(prog1)
    val prog3 = collectBulk(prog2)
    val prog4 = setLoops(prog3)
    setSkips(prog4)}
}
