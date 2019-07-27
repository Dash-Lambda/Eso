package brainfuck

import scala.annotation.tailrec

case class BlkOP(ops: Vector[(Int, Option[Int])], shift: Int){
  lazy val maxShift: Int = math.max(shift, ops.map(_._1).max)
  
  lazy val (ind, inc) = ops.head
  def doOne(p: Int, dat: Vector[Int]): (Int, Vector[Int]) = inc match{
    case Some(n) => (p + shift, dat.updated(p + ind, dat(p + ind) + n))
    case None => (p + shift, dat.updated(p + ind, 0))
  }
  
  lazy val lops: Vector[(Int, (Int, Int) => Int)] = ops.map{
    case (i, Some(n)) => (i, (x: Int, y: Int) => x + n*y)
    case (i, None) => (i, (_, _) => 0)
  }
  def doLoop(p: Int, dat: Vector[Int]): Vector[Int] = {
    val num = dat(p)
    if(dat(p) == 0) dat
    else lops.foldLeft(dat){case (vec, (i, op)) => vec.updated(p + i, op(vec(p + i), num))}
  }
  
  lazy val oops: Vector[(Int, Int => Int)] = ops.map{
    case (i, Some(n)) => (i, (x: Int) => x + n)
    case (i, None) => (i, _ => 0)
  }
  def doOp(p: Int, dat: Vector[Int]): (Int, Vector[Int]) = {
    val opd = oops.foldLeft(dat){case (vec, (i, op)) => vec.updated(p + i, op(vec(p + i)))}
    (p + shift, opd)
  }
  
  def isLoop: Boolean = (shift == 0) && !ops.contains((0, None)) && (ops.collect{case (0, Some(n)) => n}.sum == -1)
  
  override def toString: String = {
    val incs = ops
      .map{
        case (i, Some(n)) => s"$i += $n"
        case (i, None) => s"$i = 0"}
    s"($shift; ${incs.mkString(", ")})"
  }
  
  def opStr(dyn: Boolean): String = {
    val opstr = ops.map{
      case (0, Some(n)) if n > 0 => s"tape(p) += $n"
      case (0, Some(n)) if n < 0 => s"tape(p) -= ${n.abs}"
      case (i, Some(n)) if n > 0 => s"tape(p + $i) += $n"
      case (i, Some(n)) if n < 0 => s"tape(p + $i) -= ${n.abs}"
      case (i, None) => s"tape(p + $i) = 0"
    }
    s"${opstr.mkString("\n")}${if(shift != 0) s"\np += $shift" else ""}"
  }
  def lopStr(dyn: Boolean): String = {
    val opstr = ops
      .filter(_._1 != 0)
      .map{
        case (i, Some(n)) if n > 0 => s"tape(p + $i) += $n*tmp"
        case (i, Some(n)) if n < 0 => s"tape(p + $i) -= ${n.abs}*tmp"
        case (i, None) => s"tape(p + $i) = 0"
      }
    s"""|if(tape(p) != 0){
        |val tmp = tape(p)
        |${opstr.mkString("\n")}
        |tape(p) = 0
        |}""".stripMargin
  }
}
object BlkOP{
  def apply(v: Vector[(Int, Option[Int])], s: Int): BlkOP = new BlkOP(v, s)
}

object BFOptimizer {
  val ops: Vector[Char] = Vector[Char]('>', '<', '+', '-')
  val nonOps: Vector[Char] = Vector[Char]('[', ']', ',', '.', '_', 'e')
  
  def apply(progRaw: String, debug: Boolean): Option[(Vector[BlkOP], Vector[(Char, Int)])] = {
    val (bopVec, pass1) = bulk(optLazy(progRaw))
    val pass2 = loop(pass1, bopVec)
    val pass3 = skip(pass2)
    pass3.map(prog => (bopVec, prog))
  }
  
  def compOpt(progRaw: String, log: Boolean): Option[(Vector[BlkOP], Vector[(Char, Int)])] = {
    if(progRaw.count(_ == '[') == progRaw.count(_ == ']')){
      val (bopVec, pass1) = bulk(optLazy(progRaw))
      val pass2 = loop(pass1, bopVec)
      Some(bopVec -> pass2)
    }else None
  }
  
  def cont(str: String): Option[((Char, Int), String)] = str.headOption match{
    case Some(h) if ops.contains(h) => Some((h, str.takeWhile(_ == h).length) -> str.dropWhile(_ == h))
    case Some(h) => Some((h, 0) -> str.tail)
    case _ => None
  }
  def optLazy(progRaw: String): LazyList[(Char, Int)] = LazyList.unfold(progRaw.filter("><][+-,.".contains(_)) :+ 'e')(cont)
  
  def bulk(progSrc: LazyList[(Char, Int)]): (Vector[BlkOP], Vector[(Char, Int)]) = {
    @tailrec
    def bdo(ac: Vector[(Char, Int)], bac: Vector[BlkOP], vac: Vector[(Int, Option[Int])], shift: Int, ind: Int, src: LazyList[(Char, Int)]): (Vector[BlkOP], Vector[(Char, Int)]) = {
      src match{
        case ('>', n) +: tail => bdo(ac, bac, vac, shift + n, ind, tail)
        case ('<', n) +: tail => bdo(ac, bac, vac, shift - n, ind, tail)
        case ('+', n) +: tail => bdo(ac, bac, vac :+ (shift -> Some(n)), shift, ind, tail)
        case ('-', n) +: tail => bdo(ac, bac, vac :+ (shift -> Some(-n)), shift, ind, tail)
        case ('_', _) +: tail => bdo(ac, bac, vac :+ (shift -> None), shift, ind, tail)
        case p +: ps =>
          if(vac.sizeIs == 1) bdo(ac :+ ('a', ind) :+ p, bac :+ BlkOP(vac, shift), Vector[(Int, Option[Int])](), 0, ind + 1, ps)
          else if(vac.nonEmpty) bdo(ac :+ ('u', ind) :+ p, bac :+ BlkOP(vac, shift), Vector[(Int, Option[Int])](), 0, ind + 1, ps)
          else if(vac.isEmpty && shift != 0) bdo(ac :+ ('m', shift) :+ p, bac, Vector[(Int, Option[Int])](), 0, ind, ps)
          else bdo(ac :+ p, bac, vac, shift, ind, ps)
        case _ => (bac, ac)
      }
    }
    
    bdo(Vector[(Char, Int)](), Vector[BlkOP](), Vector[(Int, Option[Int])](), 0, 0, progSrc)
  }
  
  def loop(progSrc: Vector[(Char, Int)], bops: Vector[BlkOP]): Vector[(Char, Int)] = {
    @tailrec
    def ldo(ac: Vector[(Char, Int)], src: Vector[(Char, Int)]): Vector[(Char, Int)] = {
      src match{
        case ('[', _) +: ('u', addr) +: (']', _) +: tail if bops(addr).isLoop => ldo(ac :+ ('l', addr), tail)
        case ('[', _) +: ('m', stp) +: (']', _) +: tail => ldo(ac :+ ('/', stp), tail)
        case p +: ps => ldo(ac :+ p, ps)
        case _ => ac
      }
    }
    
    ldo(Vector[(Char, Int)](), progSrc)
  }
  
  def skip(progSrc: Vector[(Char, Int)]): Option[Vector[(Char, Int)]] = {
    @tailrec
    def sdo(ac: Vector[(Char, Int)], os: List[Int], ind: Int, src: Vector[(Char, Int)]): Option[Vector[(Char, Int)]] = src match{
      case p +: ps => p match{
        case ('[', _) => sdo(ac :+ p, ind +: os, ind + 1, ps)
        case (']', _) => os match{
          case i +: is => sdo(ac.updated(i, ('[', ind + 1)) :+ (']', i + 1), is, ind + 1, ps)
          case _ => None
        }
        case _ => sdo(ac :+ p, os, ind + 1, ps)
      }
      case _ if os.isEmpty => Some(ac)
      case _ => None
    }
    
    sdo(Vector[(Char, Int)](), List[Int](), 0, progSrc)
  }
}
