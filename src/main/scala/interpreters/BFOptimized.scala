package interpreters

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object BFOptimized extends Interpreter{
  val name = "BFOptimized"
  def apply(flags: Vector[Boolean], nums: Vector[Int])(progRaw: String): Try[String] = (flags, nums) match{
    case (log +: debug +: dynamicTapeSize +: _, outputMaxLength +: initTapeSize +: _) => apply(initTapeSize, outputMaxLength, dynamicTapeSize, log, debug)(progRaw)
    case _ => Failure(InterpreterException("Missing Configuration Values"))
  }
  def apply(initTapeSize: Int, outputMaxLength: Int, dynamicTapeSize: Boolean, log: Boolean, debug: Boolean)(progRaw: String): Try[String] = BFOptimizer(progRaw, debug) match{
    case Success((bops, prog)) =>
      if(debug) println(
        s"""|Optimized: ${prog.map(_._1).mkString}
            |BulkOps: [${bops.zipWithIndex.map{case (bop, i) => s"[$i, ${bop.shift}, ${bop.ops.mkString(", ")}]"}.mkString(", ")}]
            |Optimized Detail:
            |${prog.zipWithIndex.map{case ((c, n), ind) => s"$ind: $c $n\n"}.mkString}
            |""".stripMargin)
      
      if(dynamicTapeSize) bfDyn(prog, bops, initTapeSize, outputMaxLength, debug, log)
      else bfStat(prog, bops, initTapeSize, outputMaxLength, debug, log)
    case Failure(e) => Failure(e)
  }
  
  def bfStat(prog: Vector[(Char, Int)], bops: Vector[BulkOp], initTapeSize: Int, outputMaxLength: Int, debug: Boolean, log: Boolean): Try[String] = {
    def printLog(str: String): String = {if (log) print(str); str}
    
    @tailrec def scan(dat: Vector[Int], stp: Int, ind: Int): Int = if (dat(ind) != 0) scan(dat, stp, ind + stp) else ind
    def loopBulk(dat: Vector[Int], dc: Int, bop: BulkOp): Vector[Int] = {
      bop.ops.foldLeft(dat) { case (vec, (ind, num)) => vec.updated(dc + ind, vec(dc + ind) + dat(dc) * num) }
    }
    def updateBulk(dat: Vector[Int], dc: Int, bop: BulkOp): Vector[Int] = bop.ops.foldLeft(dat) { case (vec, (ind, num)) => vec.updated(dc + ind, vec(dc + ind) + num) }
    
    @tailrec
    def bfv(pc: Int, dc: Int, dat: Vector[Int], result: String): String = {
      val (op, num) = prog(pc)
      op match{
        case 'a' =>
          val bop = bops(num)
          val (ind, inc) = bop.ops.head
          bfv(pc + 1, dc + bop.shift, dat.updated(dc + ind, dat(dc + ind) + inc), result)
        case 'u' =>
          val bop = bops(num)
          bfv(pc + 1, dc + bop.shift, updateBulk(dat, dc, bop), result)
        case 'l' =>
          if(dat(dc) == 0) bfv(pc + 1, dc, dat, result)
          else bfv(pc + 1, dc, loopBulk(dat, dc, bops(num)), result)
        case 'm' => bfv(pc + 1, dc + num, dat, result)
        case '/' => bfv(pc + 1, scan(dat, num, dc), dat, result)
        case '[' => bfv(if(dat(dc) == 0) num else pc + 1, dc, dat, result)
        case ']' => bfv(if(dat(dc) == 0) pc + 1 else num, dc, dat, result)
        case ',' => bfv(pc + 1, dc, dat.updated(dc, StdIn.readInt), result)
        case '.' =>
          if((outputMaxLength == -1) || (result.sizeIs <= outputMaxLength - num)) bfv(pc + 1, dc, dat, result ++ printLog(dat(dc).toChar.toString * num))
          else result ++ printLog(dat(dc).toChar.toString * num)
        case 'e' => result
      }
    }
  
    Try{bfv(0, 0, Vector.fill(initTapeSize)(0), "")}
  }
  
  def bfDyn(prog: Vector[(Char, Int)], bops: Vector[BulkOp], initTapeSize: Int, outputMaxLength: Int, debug: Boolean, log: Boolean): Try[String] = {
    def printLog(str: String): String = {if (log) print(str); str}
    
    @tailrec def scan(dat: Vector[Int], stp: Int, ind: Int): Int = if(dat.isDefinedAt(ind) && dat(ind) != 0) scan(dat, stp, ind + stp) else ind
    def loopBulk(dat: Vector[Int], dc: Int, bop: BulkOp): Vector[Int] = bop.ops.foldLeft(dat.padTo(math.max(dc + bop.ops.last._1 + 1, dc + bop.shift + 1), 0)) { case (vec, (ind, num)) => vec.updated(dc + ind, vec(dc + ind) + dat(dc) * num) }
    def updateBulk(dat: Vector[Int], dc: Int, bop: BulkOp): Vector[Int] = bop.ops.foldLeft(dat.padTo(math.max(dc + bop.ops.last._1 + 1, dc + bop.shift + 1), 0)) { case (vec, (ind, num)) => vec.updated(dc + ind, vec(dc + ind) + num) }
    
    @tailrec
    def bfvDyn(pc: Int, dc: Int, dat: Vector[Int], result: String): String = {
      if(debug) println(
        s"""|State: PC($pc)  DC($dc)
            |${dat.mkString(" | ")}""".stripMargin)
      
      val (op, num) = prog(pc)
      op match{
        case 'a' =>
          val bop = bops(num)
          val (ind, inc) = bop.ops.head
          bfvDyn(pc + 1, dc + bop.shift, dat.padTo(math.max(dc + bop.shift + 1, dc + ind + 1), 0).updated(dc + ind, dat(dc + ind) + inc), result)
        case 'u' =>
          val bop = bops(num)
          bfvDyn(pc + 1, dc + bop.shift, updateBulk(dat, dc, bop), result)
        case 'l' =>
          if(dat(dc) == 0) bfvDyn(pc + 1, dc, dat, result)
          else bfvDyn(pc + 1, dc, loopBulk(dat, dc, bops(num)), result)
        case 'm' => bfvDyn(pc + 1, dc + num, dat.padTo(dc + num + 1, 0), result)
        case '/' =>
          val newdc = scan(dat, num, dc)
          bfvDyn(pc + 1, newdc, dat.padTo(newdc + 1, 0), result)
        case '[' => bfvDyn(if(dat(dc) == 0) num else pc + 1, dc, dat, result)
        case ']' => bfvDyn(if(dat(dc) == 0) pc + 1 else num, dc, dat, result)
        case ',' => bfvDyn(pc + 1, dc, dat.updated(dc, StdIn.readInt), result)
        case '.' =>
          if((outputMaxLength == -1) || (result.sizeIs <= outputMaxLength - num)) bfvDyn(pc + 1, dc, dat, result ++ printLog(dat(dc).toChar.toString * num))
          else result ++ printLog(dat(dc).toChar.toString * num)
        case 'e' => result
      }
    }
    
    Try{bfvDyn(0, 0, Vector.fill(initTapeSize)(0), "")}
  }
}