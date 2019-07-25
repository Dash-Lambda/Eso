package brainfuck

import common.{Interpreter, InterpreterException}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Failure, Try}

object BFOptimized extends Interpreter{
  val name = "BFOptimized"
  
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String] = {
    getParms(bools, nums)("log", "debug", "dynamicTapeSize")("outputMaxLength", "initTapeSize") match{
      case Some((log +: debug +: dynamicTapeSize +: _, outputMaxLength +: initTapeSize +: _)) =>BFOptimizer(progRaw, debug) match{
        case Some((bops, prog)) =>
          if(debug) println(
            s"""|Optimized: ${prog.map(_._1).mkString}
                |BulkOps: [${bops.zipWithIndex.map{case (bop, i) => s"[$i, ${bop.shift}, ${bop.ops.mkString(", ")}]"}.mkString(", ")}]
                |Optimized Detail:
                |${prog.zipWithIndex.map{case ((c, n), ind) => s"$ind: $c $n\n"}.mkString}
                |""".stripMargin)
          
          if(dynamicTapeSize) bfDyn(prog, bops, initTapeSize, outputMaxLength, debug, log)
          else bfStat(prog, bops, initTapeSize, outputMaxLength, debug, log)
        case None => Failure(InterpreterException("Woo! Optimizer failure! Probably mismatched brackets."))
      }
      case None => Failure(InterpreterException("Unspecified Runtime Parameters"))
    }
  }
  
  def bfStat(prog: Vector[(Char, Int)], bops: Vector[BlkOP], initTapeSize: Int, outputMaxLength: Int, debug: Boolean, log: Boolean): Try[String] = {
    @tailrec def scan(dat: Vector[Int], stp: Int, ind: Int): Int = if (dat(ind) != 0) scan(dat, stp, ind + stp) else ind
    def printLog(str: String): String = {if(log) print(str); str}
    
    @tailrec
    def bfv(pc: Int, dc: Int, dat: Vector[Int], inp: Vector[Char], result: String): String = {
      val (op, num) = prog(pc)
      op match{
        case 'a' =>
          val (ndc, ndat) = bops(num).doOne(dc, dat)
          bfv(pc + 1, ndc, ndat, inp, result)
        case 'u' =>
          val (ndc, ndat) = bops(num).doOp(dc, dat)
          bfv(pc + 1, ndc, ndat, inp, result)
        case 'l' => bfv(pc + 1, dc, bops(num).doLoop(dc, dat), inp, result)
        case 'm' => bfv(pc + 1, dc + num, dat, inp, result)
        case '/' => bfv(pc + 1, scan(dat, num, dc), dat, inp, result)
        case '[' => bfv(if(dat(dc) == 0) num else pc + 1, dc, dat, inp, result)
        case ']' => bfv(if(dat(dc) == 0) pc + 1 else num, dc, dat, inp, result)
        case ',' =>
          if(inp.nonEmpty) bfv(pc + 1, dc, dat.updated(dc, inp.head.toInt), inp.tail, result)
          else bfv(pc, dc, dat, StdIn.readLine.toVector :+ '\n', result)
        case '.' =>
          if((outputMaxLength == -1) || (result.sizeIs < outputMaxLength)) bfv(pc + 1, dc, dat, inp, result ++ printLog(dat(dc).toChar.toString))
          else result ++ printLog(dat(dc).toChar.toString)
        case 'e' => result
      }
    }
    
    Try{bfv(0, 0, Vector.fill(initTapeSize)(0), Vector[Char](), "")}
  }
  
  def bfDyn(prog: Vector[(Char, Int)], bops: Vector[BlkOP], initTapeSize: Int, outputMaxLength: Int, debug: Boolean, log: Boolean): Try[String] = {
    @tailrec def scan(dat: Vector[Int], stp: Int, ind: Int): Int = if(dat.isDefinedAt(ind) && dat(ind) != 0) scan(dat, stp, ind + stp) else ind
    def printLog(str: String): String = {if(log) print(str); str}
    
    @tailrec
    def bfv(pc: Int, dc: Int, dat: Vector[Int], inp: Vector[Char], result: String): String = {
      val (op, num) = prog(pc)
      op match{
        case 'a' =>
          val (ndc, ndat) = bops(num).doOne(dc, dat.padTo(bops(num).maxShift + dc + 1, 0))
          bfv(pc + 1, ndc, ndat, inp, result)
        case 'u' =>
          val (ndc, ndat) = bops(num).doOp(dc, dat.padTo(bops(num).maxShift + dc + 1, 0))
          bfv(pc + 1, ndc, ndat, inp, result)
        case 'l' => bfv(pc + 1, dc, bops(num).doLoop(dc, dat.padTo(bops(num).maxShift + dc + 1, 0)), inp, result)
        case 'm' => bfv(pc + 1, dc + num, dat.padTo(dc + num + 1, 0), inp, result)
        case '/' =>
          val ndc = scan(dat, num, dc)
          bfv(pc + 1, ndc, dat.padTo(ndc + 1, 0), inp, result)
        case '[' => bfv(if(dat(dc) == 0) num else pc + 1, dc, dat, inp, result)
        case ']' => bfv(if(dat(dc) == 0) pc + 1 else num, dc, dat, inp, result)
        case ',' =>
          if(inp.nonEmpty) bfv(pc + 1, dc, dat.updated(dc, inp.head.toInt), inp.tail, result)
          else bfv(pc, dc, dat, StdIn.readLine.toVector :+ '\n', result)
        case '.' =>
          if((outputMaxLength == -1) || (result.sizeIs < outputMaxLength)) bfv(pc + 1, dc, dat, inp, result ++ printLog(dat(dc).toChar.toString))
          else result ++ printLog(dat(dc).toChar.toString)
        case 'e' => result
      }
    }
    
    Try{bfv(0, 0, Vector.fill(initTapeSize)(0), Vector[Char](), "")}
  }
}
