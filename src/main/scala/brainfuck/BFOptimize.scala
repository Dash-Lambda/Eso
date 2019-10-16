package brainfuck

import common.EsoObj

import scala.annotation.tailrec
import scala.util.Try

case class BlkOp(ops: Vector[(Int, Option[Int])], shift: Int) extends EsoObj{
  lazy val maxShift: Int = math.max(shift, ops.map(_._1).max)
  lazy val (ind, inc) = ops.head
  
  def incStr(n: Int): String = s"${if(n < 0) "-" else "+"}= ${n.abs}"
  def isLoop: Boolean = (shift == 0) && !ops.contains((0, None)) && (ops.collect{case (0, Some(n)) => n}.sum == -1)
  def isMove: Boolean = ops.isEmpty
  
  override def toString: String = s"{$shift; ${ops.map{case (i, arg) => s"$i -> ${arg match{case Some(n) => n.toString; case None => "_"}}"}.mkString(", ")}}"
  
  def doLoop(p: Int, dat: Vector[Int]): Vector[Int] = {
    val num = dat(p)
    if(num == 0) dat
    else ops.foldLeft(dat){
      case (ac, (i, Some(n))) => ac.updated(p + i, ac(p + i) + num*n)
      case (ac, (i, None)) => ac.updated(p + i, 0)
    }
  }
  
  def doOp(p: Int, dat: Vector[Int]): Vector[Int] = ops.foldLeft(dat){
    case (ac, (i, Some(n))) => ac.updated(p + i, ac(p + i) + n)
    case (ac, (i, None)) => ac.updated(p + i, 0)
  }
  
  def opStr: String = {
    val opstr = ops.map{
      case (0, Some(n)) => s"tape(p) ${incStr(n)}"
      case (i, Some(n)) => s"tape(p + $i) ${incStr(n)}"
      case (i, None) => s"tape(p + $i) = 0"
    }
    s"${opstr.mkString("\n")}${if(shift != 0) s"\np ${incStr(shift)}" else ""}"
  }
  def lopStr: String = {
    val opstr = ops
      .filter(_._1 != 0)
      .map{
        case (i, Some(n)) => s"tape(p + $i) ${incStr(n)}*tmp"
        case (i, None) => s"tape(p + $i) = 0"
      }
    s"""|if(tape(p) != 0){
        |val tmp = tape(p)
        |${opstr.mkString("\n")}
        |tape(p) = 0
        |}""".stripMargin
  }
}

object BFOptimize extends EsoObj{
  val blks: Vector[Char] = Vector('<', '>', '+', '-', '_')
  val ops: Vector[Char] = Vector('<', '>', '+', '-')
  
  def apply(progRaw: String): Try[Vector[(Char, Either[Int, BlkOp])]] = Try{optSkip(optLoop(optBulk(optBase(progRaw))))}
  def compOpt(progRaw: String): Try[LazyList[(Char, Either[Int, BlkOp])]] = Try{optLoop(optBulk(optBase(progRaw)))}
  
  def optBase(progRaw: String): LazyList[(Char, Int)] = LazyList.unfold(progRaw.filter("><][+-,.".contains(_)).replaceAll("""\[[+\-]\]""", "_") :+ 'e')(cont)
  def optBulk(prog: LazyList[(Char, Int)]): LazyList[(Char, Either[Int, BlkOp])] = LazyList.unfold(prog)(bulk)
  def optLoop(prog: LazyList[(Char, Either[Int, BlkOp])]): LazyList[(Char, Either[Int, BlkOp])] = LazyList.unfold(prog)(loop)
  def optSkip(prog: LazyList[(Char, Either[Int, BlkOp])]): Vector[(Char, Either[Int, BlkOp])] = {
    @tailrec
    def sdo(ac: Vector[(Char, Either[Int, BlkOp])], stack: List[Int], i: Int, src: LazyList[(Char, Either[Int, BlkOp])]): Vector[(Char, Either[Int, BlkOp])] = src match{
      case ('[', _) +: ops => sdo(ac :+ ('[', Left(0)), i +: stack, i + 1, ops)
      case (']', _) +: ops => sdo(ac.updated(stack.head, ('[', Left(i + 1))) :+ ((']', Left(stack.head + 1))), stack.tail, i + 1, ops)
      case op +: ops => sdo(ac :+ op, stack, i + 1, ops)
      case _ => ac
    }
    sdo(Vector[(Char, Either[Int, BlkOp])](), List[Int](), 0, prog)
  }
  
  def cont(str: String): Option[((Char, Int), String)] = str.headOption.map{h =>
    if(ops.contains(h)) (h, str.takeWhile(_ == h).length) -> str.dropWhile(_ == h)
    else (h, 0) -> str.tail
  }
  
  def bulk(src: LazyList[(Char, Int)]): Option[((Char, Either[Int, BlkOp]), LazyList[(Char, Int)])] = {
    @tailrec
    def ado(ac: Vector[(Int, Option[Int])], s: Int, src: Vector[(Char, Int)]): (Vector[(Int, Option[Int])], Int) = src match{
      case ('+', n) +: ops => ado(ac :+ (s, Some(n)), s, ops)
      case ('-', n) +: ops => ado(ac :+ (s, Some(-n)), s, ops)
      case ('_', _) +: ops => ado(ac :+ (s, None), s, ops)
      case ('>', n) +: ops => ado(ac, s + n, ops)
      case ('<', n) +: ops => ado(ac, s - n, ops)
      case _ => (ac, s)
    }
    
    src match{
      case (op, _) +: _ if blks.contains(op) =>
        val (v, s) = ado(Vector[(Int, Option[Int])](), 0, src.takeWhile(p => blks.contains(p._1)).toVector)
        Some((('u', Right(BlkOp(v, s))), src.dropWhile(p => blks.contains(p._1))))
      case (op, n) +: tail => Some(((op, Left(n)), tail))
      case _ => None
    }
  }
  
  def loop(src: LazyList[(Char, Either[Int, BlkOp])]): Option[((Char, Either[Int, BlkOp]), LazyList[(Char, Either[Int, BlkOp])])] = src match{
    case ('[', _) +: ('u', Right(bop)) +: (']', _) +: tail if bop.isLoop => Some((('l', Right(bop)), tail))
    case ('[', _) +: ('u', Right(bop)) +: (']', _) +: tail if bop.isMove => Some((('/', Left(bop.shift)), tail))
    case ('u', Right(bop)) +: tail if bop.isMove => Some((('m', Left(bop.shift)), tail))
    case p +: ps => Some((p, ps))
    case _ => None
  }
}
