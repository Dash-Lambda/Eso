package interpreters

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object BFFunctional extends Interpreter {
  val name = "BFBase"
  
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String] = {
    getParms(bools, nums)("log", "debug", "dynamicTapeSize")("outputMaxLength", "initTapeSize") match{
      case Some((log +: debug +: dynamicTapeSize +: _, outputMaxLength +: initTapeSize +: _)) => bfFunc(initTapeSize, outputMaxLength, dynamicTapeSize, log, debug)(progRaw)
      case None => Failure(InterpreterException("Unspecified Runtime Parameters"))
    }
  }
  
  def bfFunc(initTapeSize: Int, outputMaxLength: Int, dynamicTapeSize: Boolean, log: Boolean, debug: Boolean)(prog: String): Try[String] = {
    @tailrec
    def bfi(plog: String, psrc: String, dlog: List[Int], dsrc: List[Int], dir: Int, cnt: Int, inp: Vector[Char], result: String): Try[String] = dir match{
      case 1 => (psrc.headOption, cnt) match{
        case (Some(']'), 0) => bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, 0, 0, inp, result)
        case (Some(']'), _) => bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt - 1, inp, result)
        case (Some('['), _) => bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt + 1, inp, result)
        case (Some(_), _) => bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt, inp, result)
        case _ => Failure(InterpreterException("Skip Forward Exception"))
      }
      case -1 => (plog.headOption, cnt) match{
        case (Some('['), 0) => bfi(plog, psrc, dlog, dsrc, 0, 0, inp, result)
        case (Some('['), _) => bfi(plog.tail, plog.head +: psrc, dlog, dsrc, dir, cnt - 1, inp, result)
        case (Some(']'), _) => bfi(plog.tail, plog.head +: psrc, dlog, dsrc, dir, cnt + 1, inp, result)
        case (Some(_), _) => bfi(plog.tail, plog.head +: psrc, dlog, dsrc, dir, cnt, inp, result)
        case _ => Failure(InterpreterException("Loop Back Exception"))
      }
      case 0 => psrc.headOption match{
        case Some(op) => op match{
          case '>' =>
            if(dsrc.sizeIs > 1) bfi(psrc.head +: plog, psrc.tail, dsrc.head +: dlog, dsrc.tail, dir, cnt, inp, result)
            else if(dynamicTapeSize) bfi(psrc.head +: plog, psrc.tail, dsrc.head +: dlog, List(0), dir, cnt, inp, result)
            else Failure(InterpreterException("End of Tape"))
          case '<' => if(dlog.nonEmpty) bfi(psrc.head +: plog, psrc.tail, dlog.tail, dlog.head +: dsrc, dir, cnt, inp, result) else Failure(InterpreterException("Beginning of Tape Reached"))
          case '+' => bfi(psrc.head +: plog, psrc.tail, dlog, (dsrc.head + 1) +: dsrc.tail, dir, cnt, inp, result)
          case '-' => bfi(psrc.head +: plog, psrc.tail, dlog, (dsrc.head - 1) +: dsrc.tail, dir, cnt, inp, result)
          case ']' => if(dsrc.head == 0) bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt, inp, result) else bfi(plog.tail, plog.head +: psrc, dlog, dsrc, -1, 0, inp, result)
          case '[' => if(dsrc.head == 0) bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, 1, 0, inp, result) else bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt, inp, result)
          case '.' => if(log) print(dsrc.head.toChar); if((outputMaxLength == -1) || (result.length + 1 < outputMaxLength)) bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt, inp, result :+ dsrc.head.toChar) else Success(result :+ dsrc.head.toChar)
          case ',' =>
            if(inp.nonEmpty) bfi(psrc.head +: plog, psrc.tail, dlog, inp.head +: dsrc.tail, dir, cnt, inp.tail, result)
            else bfi(plog, psrc, dlog, dsrc, dir, cnt, StdIn.readLine.toVector :+ '\n', result)
        }
        case _ => Success(result)
      }
    }
    
    bfi("", prog.filter("<>+-.,[]".contains(_)), List[Int](), List.fill(initTapeSize)(0), 0, 0, Vector[Char](), "")
  }
}