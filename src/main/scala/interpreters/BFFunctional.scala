package interpreters

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object BFFunctional {
  def bfRun(prog: String): Option[String] = bfRun(prog, log = true)
  def bfRun(prog: String, log: Boolean): Option[String] = {
    @tailrec
    def bfi(plog: String, psrc: String, dlog: List[Int], dsrc: List[Int], dir: Int, cnt: Int, result: String): Option[String] = dir match{
      case 1 => (psrc.headOption, cnt) match{
        case (Some(']'), 0) => bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, 0, 0, result)
        case (Some(']'), _) => bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt - 1, result)
        case (Some('['), _) => bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt + 1, result)
        case (Some(_), _) => bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt, result)
        case _ => None
      }
      case -1 => (plog.headOption, cnt) match{
        case (Some('['), 0) => bfi(plog, psrc, dlog, dsrc, 0, 0, result)
        case (Some('['), _) => bfi(plog.tail, plog.head +: psrc, dlog, dsrc, dir, cnt - 1, result)
        case (Some(']'), _) => bfi(plog.tail, plog.head +: psrc, dlog, dsrc, dir, cnt + 1, result)
        case (Some(_), _) => bfi(plog.tail, plog.head +: psrc, dlog, dsrc, dir, cnt, result)
        case _ => None
      }
      case 0 => psrc.headOption match{
        case Some(op) => op match{
          case '>' => if(dsrc.tail.nonEmpty) bfi(psrc.head +: plog, psrc.tail, dsrc.head +: dlog, dsrc.tail, dir, cnt, result) else bfi(psrc.head +: plog, psrc.tail, dsrc.head +: dlog, List(0), dir, cnt, result)
          case '<' => if(dlog.nonEmpty) bfi(psrc.head +: plog, psrc.tail, dlog.tail, dlog.head +: dsrc, dir, cnt, result) else None
          case '+' => bfi(psrc.head +: plog, psrc.tail, dlog, (dsrc.head + 1) +: dsrc.tail, dir, cnt, result)
          case '-' => bfi(psrc.head +: plog, psrc.tail, dlog, (dsrc.head - 1) +: dsrc.tail, dir, cnt, result)
          case ']' => if(dsrc.head == 0) bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt, result) else bfi(plog.tail, plog.head +: psrc, dlog, dsrc, -1, 0, result)
          case '[' => if(dsrc.head == 0) bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, 1, 0, result) else bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt, result)
          case '.' => if(log) print(dsrc.head.toChar); bfi(psrc.head +: plog, psrc.tail, dlog, dsrc, dir, cnt, result :+ dsrc.head.toChar)
          case ',' => Try(scala.io.StdIn.readInt) match{
            case Success(inp) => bfi(psrc.head +: plog, psrc.tail, dlog, inp +: dsrc.tail, dir, cnt, result)
            case Failure(_) => bfi(plog, psrc, dlog, dsrc, dir, cnt, result)
          }
        }
        case _ => Some(result)
      }
    }
    
    bfi("", prog.filter("<>+-.,[]".contains(_)), List[Int](), List[Int](0), 0, 0, "")
  }
}