package interpreters

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object BFFunctional extends Interpreter {
  def apply(log: Boolean, debug: Boolean)(prog: String): Try[String] = apply(1, -1, log, debug)(prog)
  def apply(initTapeSize: Int, outputMaxLength: Int, log: Boolean, debug: Boolean)(prog: String): Try[String] = {
    @tailrec
    def bfi(plog: String, psrc: String, dlog: List[Int], dsrc: List[Int], dir: Int, cnt: Int, result: String): Try[String] = dir match{
      case 1 => (psrc.headOption, cnt) match{
        case (Some(']'), 0) => bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, 0, 0, result)
        case (Some(']'), _) => bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt - 1, result)
        case (Some('['), _) => bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt + 1, result)
        case (Some(_), _) => bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt, result)
        case _ => Failure(InterpreterException("Skip Forward Exception"))
      }
      case -1 => (plog.headOption, cnt) match{
        case (Some('['), 0) => bfi(plog, psrc, dlog, dsrc, 0, 0, result)
        case (Some('['), _) => bfi(plog.tail, plog.head +: psrc, dlog, dsrc, dir, cnt - 1, result)
        case (Some(']'), _) => bfi(plog.tail, plog.head +: psrc, dlog, dsrc, dir, cnt + 1, result)
        case (Some(_), _) => bfi(plog.tail, plog.head +: psrc, dlog, dsrc, dir, cnt, result)
        case _ => Failure(InterpreterException("Loop Back Exception"))
      }
      case 0 => psrc.headOption match{
        case Some(op) => op match{
          case '>' => if(dsrc.tail.nonEmpty) bfi(psrc.head +: plog, psrc.tail, dsrc.head +: dlog, dsrc.tail, dir, cnt, result) else bfi(psrc.head +: plog, psrc.tail, dsrc.head +: dlog, List(0), dir, cnt, result)
          case '<' => if(dlog.nonEmpty) bfi(psrc.head +: plog, psrc.tail, dlog.tail, dlog.head +: dsrc, dir, cnt, result) else Failure(InterpreterException("Beginning of Tape Reached"))
          case '+' => bfi(psrc.head +: plog, psrc.tail, dlog, (dsrc.head + 1) +: dsrc.tail, dir, cnt, result)
          case '-' => bfi(psrc.head +: plog, psrc.tail, dlog, (dsrc.head - 1) +: dsrc.tail, dir, cnt, result)
          case ']' => if(dsrc.head == 0) bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt, result) else bfi(plog.tail, plog.head +: psrc, dlog, dsrc, -1, 0, result)
          case '[' => if(dsrc.head == 0) bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, 1, 0, result) else bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt, result)
          case '.' => if(log) print(dsrc.head.toChar); if((outputMaxLength == -1) || (result.length + 1 < outputMaxLength)) bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt, result :+ dsrc.head.toChar) else Success(result :+ dsrc.head.toChar)
          case ',' => bfi(psrc.head +: plog, psrc.tail, dlog, StdIn.readInt +: dsrc.tail, dir, cnt, result)
        }
        case _ => Success(result)
      }
    }
    
    bfi("", prog.filter("<>+-.,[]".contains(_)), List[Int](), List.fill(initTapeSize)(0), 0, 0, "")
  }
}