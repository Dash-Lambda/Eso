package interpreters

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object BFOptimized extends Interpreter{
  def apply(log: Boolean, debug: Boolean)(prog: String): Try[String] = apply(1, -1, log, debug)(prog)
  def apply(initTapeSize: Int, outputMaxLength: Int, log: Boolean, debug: Boolean)(prog: String): Try[String] = optimize(prog) match{
    case Success(optProg) => bfoFunc(optProg, initTapeSize, outputMaxLength, log, debug)
    case Failure(e) => Failure(e)
  }
  
  def bfoFunc(prog: Vector[(Char, Int)], initTapeSize: Int, outputMaxLength: Int, log: Boolean, debug: Boolean): Try[String] = {
    def printLog(str: String): String = {if(log) print(str); str}
    def scan(vec: Vector[Int], stp: Int): Int = LazyList.from(0, stp).takeWhile(n => (vec.sizeIs > n) && (vec(n) != 0)).lastOption match{
      case Some(len) => len + stp
      case None => 0
    }
    
    @tailrec
    def bfo(plog: Vector[(Char, Int)], psrc: Vector[(Char, Int)], dlog: Vector[Int], dsrc: Vector[Int], result: String): String = psrc match{
      case p +: ps =>
        val (c, n) = p
        if(debug) println(s"BFO: $c $n [${dlog.mkString(" | ")} -(${if(dsrc.nonEmpty) dsrc.head.toString else ""})- ${dsrc.tail.mkString(" | ")}]")
        c match{
          case '>' =>
            if(dsrc.sizeIs > n) bfo(plog :+ p, ps, dlog ++ dsrc.take(n), dsrc.drop(n), result)
            else bfo(plog :+ p, ps, dlog ++ dsrc.padTo(n, 0), Vector(0), result)
          case '<' => bfo(plog :+ p, ps, dlog.dropRight(n), dlog.takeRight(n) ++ dsrc, result)
          case '+' => bfo(plog :+ p, ps, dlog, (dsrc.head + n) +: dsrc.tail, result)
          case '-' => bfo(plog :+ p, ps, dlog, (dsrc.head - n) +: dsrc.tail, result)
          case '[' =>
            if(dsrc.head == 0) bfo(plog ++ psrc.take(n), psrc.drop(n), dlog, dsrc, result)
            else bfo(plog :+ p, ps, dlog, dsrc, result)
          case ']' =>
            if(dsrc.head == 0) bfo(plog :+ p, ps, dlog, dsrc, result)
            else bfo(plog.take(n), plog.drop(n) ++ psrc, dlog, dsrc, result)
          case '.' =>
            if(outputMaxLength < 1 || result.length + n < outputMaxLength) bfo(plog :+ p, ps, dlog, dsrc, result ++ printLog(dsrc.head.toChar.toString * n))
            else result ++ (dsrc.head.toChar.toString * n)
          case ',' => bfo(plog :+ p, ps, dlog, StdIn.readInt +: dsrc.tail, result)
          case '/' =>
            val len = scan(dsrc, n)
            if(len < dsrc.length) bfo(plog :+ p, ps, dlog ++ dsrc.take(len), dsrc.drop(len), result)
            else bfo(plog :+ p, ps, dlog ++ dsrc.padTo(len, 0), Vector(0), result)
          case '\\' =>
            val len = scan(dsrc.head +: dlog.reverse, n)
            bfo(plog :+ p, ps, dlog.dropRight(len), dlog.takeRight(len) ++ dsrc, result)
          case 'r' =>
            val (inc, num) = ps.head
            val signum = if(inc == '+') 1 else -1
            if(dsrc.sizeIs > n){
              val update = dsrc(n) + (signum*num*dsrc.head)
              bfo(plog ++ psrc.take(2), psrc.drop(2), dlog, 0 +: dsrc.updated(n, update).tail, result)
            }else{
              val update = signum*num*dsrc.head
              val newTape = 0 +: dsrc.tail.padTo(n - 1, 0) :+ update
              bfo(plog ++ psrc.take(2), psrc.drop(2), dlog, newTape, result)
            }
          case 'l' =>
            if(dsrc.head != 0){
              val (inc, num) = ps.head
              val signum = if(inc == '+') 1 else -1
              val ptr = dlog.length - n
              val update = dlog(ptr) + (signum*num*dsrc.head)
              bfo(plog ++ psrc.take(2), psrc.drop(2), dlog.updated(ptr, update), 0 +: dsrc.tail, result)
            } else{ bfo(plog ++ psrc.take(2), psrc.drop(2), dlog, dsrc, result)}
          case '_' => bfo(plog :+ p, ps, dlog, 0 +: dsrc.tail, result)
        }
      case _ => result
    }
    
    Try{bfo(Vector[(Char, Int)](), prog, Vector[Int](), Vector.fill(initTapeSize)(0), "")}
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
      case ('[', _) +: ps => optSkip(ac :+ ('[', optScrub(ps, 0, 2)), ps, ac.length +: bStack)
      case (']', _) +: ps => bStack.headOption match{
        case Some(i) => optSkip(ac :+ (']', i), ps, bStack.tail)
        case None => throw InterpreterException("Brakcet Mismatch")
      }
      case p +: ps => optSkip(ac :+ p, ps, bStack)
      case _ => ac
    }
    
    @tailrec
    def optScan(ac: Vector[(Char, Int)], src: Vector[(Char, Int)]): Vector[(Char, Int)] = src match{
      case ('[', addr) +: ps => ps match{
        case ('>', n) +: (']', _) +: tail => optScan(ac :+ ('/', n), tail)
        case ('<', n) +: (']', _) +: tail => optScan(ac :+ ('\\', n), tail)
        case _ => optScan(ac :+ ('[', addr), ps)
      }
      case p +: ps => optScan(ac :+ p, ps)
      case _ => ac
    }
    
    @tailrec
    def optClear(ac: Vector[(Char, Int)], src: Vector[(Char, Int)]): Vector[(Char, Int)] = src match{
      case ('[', _) +: ('-', _) +: (']', _) +: tail => optClear(ac :+ ('_', 0), tail)
      case p +: ps => optClear(ac :+ p, ps)
      case _ => ac
    }
    
    @tailrec
    def optCopy(ac: Vector[(Char, Int)], src: Vector[(Char, Int)]): Vector[(Char, Int)] = src match{
      case ('[', _) +: ('-', 1) +: ('>', l1) +: (m, n) +: ('<', l2) +: (']', _) +: tail if l1 == l2 && (m == '+' || m == '-') => optCopy(ac :+ ('r', l1) :+ (m, n), tail)
      case ('[', _) +: ('-', 1) +: ('<', l1) +: (m, n) +: ('>', l2) +: (']', _) +: tail if l1 == l2 && (m == '+' || m == '-') => optCopy(ac :+ ('l', l1) :+ (m, n), tail)
      case ('[', _) +: ('>', l1) +: (m, n) +: ('<', l2) +: ('-', 1) +: (']', _) +: tail if l1 == l2 && (m == '+' || m == '-') => optCopy(ac :+ ('r', l1) :+ (m, n), tail)
      case ('[', _) +: ('<', l1) +: (m, n) +: ('>', l2) +: ('-', 1) +: (']', _) +: tail if l1 == l2 && (m == '+' || m == '-') => optCopy(ac :+ ('l', l1) :+ (m, n), tail)
      case p +: ps => optCopy(ac :+ p, ps)
      case _ => ac
    }
    
    val emptyAC = Vector[(Char, Int)]()
    val pass1 = optBase(emptyAC, prog.filter(c => ops.contains(c) || nonOps.contains(c)))
    val pass2 = optScan(emptyAC, pass1)
    val pass3 = optClear(emptyAC, pass2)
    val pass4 = optCopy(emptyAC, pass3)
    optSkip(emptyAC, pass4, Vector[Int]())
  }
}