package interpreters

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object BFOptimized {
  def bfoRun(prog: String): Try[String] = bfoFunc(optimize(prog, log = false), 100, log = true)
  def bfoFunc(prog: Vector[(Char, Int)], initTapeSize: Int, log: Boolean): Try[String] = {
    @tailrec
    def skip(ac: Vector[(Char, Int)], src: Vector[(Char, Int)], cnt: Int): Try[(Vector[(Char, Int)], Vector[(Char, Int)])] = src match{
      case p +: ps =>
        (p, cnt) match{
          case ((']', 1), 0) => Success(ac :+ p, ps)
          case ((']', 1), _) => skip(ac :+ p, ps, cnt - 1)
          case (('[', 1), _) => skip(ac :+ p, ps, cnt + 1)
          case (_, _) => skip(ac :+ p, ps, cnt)
        }
      case _ => Failure(InterpreterException("Loop Forward Exception"))
    }
    
    @tailrec
    def bfo(plog: Vector[(Char, Int)], psrc: Vector[(Char, Int)], dlog: Vector[Int], dsrc: Vector[Int], bStack: Vector[Int], result: String): Try[String] = psrc match{
      case p +: ps =>
        val (c, n) = p
        c match{
          case '>' =>
            if(dsrc.sizeIs > n) bfo(plog :+ p, ps, dlog ++ dsrc.take(n), dsrc.drop(n), bStack, result)
            else bfo(plog :+ p, ps, dlog ++ dsrc.padTo(n, 0), Vector(0), bStack, result)
          case '<' =>
            if(dlog.sizeIs >= n) bfo(plog :+ p, ps, dlog.dropRight(n), dlog.takeRight(n) ++ dsrc, bStack, result)
            else Failure(InterpreterException("Beginning of Tape Reached"))
          case '+' => bfo(plog :+ p, ps, dlog, (dsrc.head + n) +: dsrc.tail, bStack, result)
          case '-' => bfo(plog :+ p, ps, dlog, (dsrc.head - n) +: dsrc.tail, bStack, result)
          case '[' =>
            if(dsrc.head == 0) skip(Vector[(Char, Int)](), ps, 0) match{
              case Success((ac, src)) => bfo(plog  ++ (p +: ac), src, dlog, dsrc, bStack, result)
              case Failure(e) => Failure(e)
            }
            else bfo(plog :+ p, ps, dlog, dsrc, plog.length +: bStack, result)
          case ']' =>
            if(dsrc.head == 0) bfo(plog :+ p, ps, dlog, dsrc, bStack.tail, result)
            else bStack.headOption match{
              case Some(i) => bfo(plog.take(i + 1), plog.drop(i + 1) ++ psrc, dlog, dsrc, bStack, result)
              case None => Failure(InterpreterException("Loop Back Exception"))
            }
          case '.' =>
            val out = dsrc.head.toChar
            if(log) print(out)
            bfo(plog :+ p, ps, dlog, dsrc, bStack, result :+ out)
          case ',' => Try{StdIn.readInt} match{
            case Success(inp) => bfo(plog :+ p, ps, dlog, inp +: dsrc.tail, bStack, result)
            case Failure(e) => Failure(InterpreterException(s"Input Exception ($e)"))
          }
        }
      case _ => Success(result)
    }
    bfo(Vector[(Char, Int)](), prog, Vector[Int](), Vector.fill(initTapeSize)(0), Vector[Int](), "")
  }
  
  def optimize(prog: String, log: Boolean): Vector[(Char, Int)] = {
    val ops = Vector[Char]('>', '<', '+', '-')
    val nonOps = Vector[Char]('[', ']', ',', '.')
    
    @tailrec
    def oHelper(ac: Vector[(Char, Int)], src: String): Vector[(Char, Int)] = {
      if(log) println(s"- Opt: ${ac.lastOption match{case Some(p) => p; case None => "-"}} $src")
      src.headOption match{
        case Some(c) =>
          if(ops.contains(c)) oHelper(ac :+ (c, src.takeWhile(_ == c).length), src.dropWhile(_ == c))
          else if(nonOps.contains(c)) oHelper(ac :+ (c, 1), src.tail)
          else oHelper(ac, src.dropWhile(ch => !(ops.contains(ch) || nonOps.contains(c))))
        case None => ac
      }
    }
    oHelper(Vector[(Char, Int)](), prog.filter(c => ops.contains(c) || nonOps.contains(c)))
  }
}
