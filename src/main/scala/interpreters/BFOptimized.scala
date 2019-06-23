package interpreters

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object BFOptimized {
  def bfoRun(prog: String, initTapeSize: Int, outputMaxLength: Int, log: Boolean): Try[String] = {
    optimize(prog) match{
      case Success(optProg) => bfoFunc(optProg, initTapeSize, outputMaxLength, log)
      case Failure(e) => Failure(e)
    }
  }
  def bfoFunc(prog: Vector[(Char, Int)], initTapeSize: Int, outputMaxLength: Int, log: Boolean): Try[String] = {
    @tailrec
    def bfo(plog: Vector[(Char, Int)], psrc: Vector[(Char, Int)], dlog: Vector[Int], dsrc: Vector[Int], result: String): Try[String] = psrc match{
      case p +: ps =>
        val (c, n) = p
        c match{
          case '>' =>
            if(dsrc.sizeIs > n) bfo(plog :+ p, ps, dlog ++ dsrc.take(n), dsrc.drop(n), result)
            else bfo(plog :+ p, ps, dlog ++ dsrc.padTo(n, 0), Vector(0), result)
          case '<' =>
            if(dlog.sizeIs >= n) bfo(plog :+ p, ps, dlog.dropRight(n), dlog.takeRight(n) ++ dsrc, result)
            else Failure(InterpreterException("Beginning of Tape Reached"))
          case '+' => bfo(plog :+ p, ps, dlog, (dsrc.head + n) +: dsrc.tail, result)
          case '-' => bfo(plog :+ p, ps, dlog, (dsrc.head - n) +: dsrc.tail, result)
          case '[' =>
            if(dsrc.head == 0) bfo(plog ++ psrc.take(n), psrc.drop(n), dlog, dsrc, result)
            else bfo(plog :+ p, ps, dlog, dsrc, result)
          case ']' =>
            if(dsrc.head == 0) bfo(plog :+ p, ps, dlog, dsrc, result)
            else bfo(plog.take(n), plog.drop(n) ++ psrc, dlog, dsrc, result)
          case '.' =>
            val out = dsrc.head.toChar.toString * n
            if(log) print(out)
            if((outputMaxLength == -1) || (result.length + n < outputMaxLength)) bfo(plog :+ p, ps, dlog, dsrc, result ++ out)
            else Success(result ++ out)
          case ',' => Try{StdIn.readInt} match{
            case Success(inp) => bfo(plog :+ p, ps, dlog, inp +: dsrc.tail, result)
            case Failure(e) => Failure(InterpreterException(s"Input Exception ($e)"))
          }
        }
      case _ => Success(result)
    }
    bfo(Vector[(Char, Int)](), prog, Vector[Int](), Vector.fill(initTapeSize)(0), "")
  }
  
  def optimize(prog: String): Try[Vector[(Char, Int)]] = Try{
    val ops = Vector[Char]('>', '<', '+', '-', '.')
    val nonOps = Vector[Char]('[', ']', ',')
    
    @tailrec
    def optBase(ac: Vector[(Char, Int)], src: String): Vector[(Char, Int)] = src.headOption match{
      case Some(c) =>
        if(ops.contains(c)) optBase(ac :+ (c, src.takeWhile(_ == c).length), src.dropWhile(_ == c))
        else if(nonOps.contains(c)) optBase(ac :+ (c, 1), src.tail)
        else optBase(ac, src.dropWhile(ch => !(ops.contains(ch) || nonOps.contains(c))))
      case None => ac
    }
    
    @tailrec
    def optScrub(src: Vector[(Char, Int)], cnt: Int, len: Int): Int = src match{
      case (']', _) +: _ if cnt == 0 => len
      case ('[', _) +: ps => optScrub(ps, cnt + 1, len + 1)
      case (']', _) +: ps => optScrub(ps, cnt - 1, len + 1)
      case (_, _) +: ps => optScrub(ps, cnt, len + 1)
      case _ => throw InterpreterException("Bracket Mismatch")
    }
    
    @tailrec
    def optSkip(ac: Vector[(Char, Int)], src: Vector[(Char, Int)], bStack: Vector[Int]): Vector[(Char, Int)] = src match{
      case ('[', _) +: ps =>
        val jump = optScrub(ps, 0, 2)
        optSkip(ac :+ ('[', jump), ps, ac.length +: bStack)
      case (']', _) +: ps =>
        bStack.headOption match{
          case Some(i) => optSkip(ac :+ (']', i), ps, bStack.tail)
          case None => throw InterpreterException("Brakcet Mismatch")
        }
      case p +: ps => optSkip(ac :+ p, ps, bStack)
      case _ => ac
    }
    
    val emptyAC = Vector[(Char, Int)]()
    optSkip(emptyAC, optBase(emptyAC, prog.filter(c => ops.contains(c) || nonOps.contains(c))), Vector[Int]())
  }
}
