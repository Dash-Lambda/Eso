package interpreters

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object BFFunctional {
  def bfRun(prog: String): Option[String] = bfRun(prog, true)
  def bfRun(prog: String, log: Boolean): Option[String] = bfi("", prog.filter("<>+-.,[]".contains(_)), List[Int](), List[Int](0), 0, 0, log, "")
  
  @tailrec
  def bfi(plog: String, psrc: String, dlog: List[Int], dsrc: List[Int], dir: Int, cnt: Int, cons: Boolean, result: String): Option[String] = dir match{
    case 1 => (psrc.headOption, cnt) match{
      case (Some(']'), 0) => bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, 0, 0, cons, result)
      case (Some(']'), _) => bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt - 1, cons, result)
      case (Some('['), _) => bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt + 1, cons, result)
      case (Some(_), _) => bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt, cons, result)
      case _ => None
    }
    case -1 => (plog.headOption, cnt) match{
      case (Some('['), 0) => bfi(plog, psrc, dlog, dsrc, 0, 0, cons, result)
      case (Some('['), _) => bfi(plog.tail, plog.head +: psrc, dlog, dsrc, dir, cnt - 1, cons, result)
      case (Some(']'), _) => bfi(plog.tail, plog.head +: psrc, dlog, dsrc, dir, cnt + 1, cons, result)
      case (Some(_), _) => bfi(plog.tail, plog.head +: psrc, dlog, dsrc, dir, cnt, cons, result)
      case _ => None
    }
    case 0 => psrc.headOption match{
      case Some(op) => op match{
        case '>' => if(dsrc.tail.nonEmpty) bfi(psrc.head +: plog, psrc.tail, dsrc.head +: dlog, dsrc.tail, dir, cnt, cons, result) else bfi(psrc.head +: plog, psrc.tail, dsrc.head +: dlog, List(0), dir, cnt, cons, result)
        case '<' => if(dlog.nonEmpty) bfi(psrc.head +: plog, psrc.tail, dlog.tail, dlog.head +: dsrc, dir, cnt, cons, result) else None
        case '+' => bfi(psrc.head +: plog, psrc.tail, dlog, (dsrc.head + 1) +: dsrc.tail, dir, cnt, cons, result)
        case '-' => bfi(psrc.head +: plog, psrc.tail, dlog, (dsrc.head - 1) +: dsrc.tail, dir, cnt, cons, result)
        case ']' => if(dsrc.head == 0) bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt, cons, result) else bfi(plog.tail, plog.head +: psrc, dlog, dsrc, -1, 0, cons, result)
        case '[' => if(dsrc.head == 0) bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, 1, 0, cons, result) else bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt, cons, result)
        case '.' => if(cons) print(dsrc.head.toChar); bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt, cons, result :+ dsrc.head.toChar)
        case ',' => Try(scala.io.StdIn.readInt) match{
          case Success(inp) => bfi(psrc.head +: plog, psrc.tail, dlog, inp +: dsrc.tail, dir, cnt, cons, result)
          case Failure(_) => bfi(plog, psrc, dlog, dsrc, dir, cnt, cons, result)
        }
      }
      case _ => Some(result)
    }
  }
}