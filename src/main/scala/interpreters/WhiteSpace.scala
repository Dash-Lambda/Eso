package interpreters

import scala.annotation.tailrec
import scala.collection.immutable
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object WhiteSpace extends Interpreter {
  val lnfe = Failure(InterpreterException("Label Not Found"))
  val eose = Failure(InterpreterException("End of Stack"))
  val nihe = Failure(InterpreterException("Not In Heap"))
  val snfe = Failure(InterpreterException("Subroutine Not Found"))
  val eope = Failure(InterpreterException("End of Program Reached"))
  
  def apply(log: Boolean)(progRaw: String): Try[String] = {
    val prog = formatProg(progRaw)
    val calls = prog.zipWithIndex
      .filter{case ((op, _), _) => op == "label"}
      .map{case ((_, num), ind) => (num, ind)}
    val callMap = {
      val builder = immutable.HashMap.newBuilder[Long, Int]
      builder ++= calls
      builder.result
    }
    apply(prog, callMap, log)
  }
  
  def apply(prog: Vector[(String, Long)], callAddrs: immutable.HashMap[Long, Int], log: Boolean): Try[String] = {
    @tailrec
    def wsi(pc: Int, stack: List[Long], heap: immutable.HashMap[Long, Long], callStack: List[Int], result: String): Try[String] = {
      prog(pc) match{
        case (op, num) =>
          //if(!log) println(f"$op%10s: $num {${stack.mkString(" | ")}} [${heap.mkString(" | ")}] (${callStack.mkString(" | ")})")
          op match{
            case "push" => wsi(pc + 1, num +: stack, heap, callStack, result)
            case "dup" => stack match{
              case n +: ns => wsi(pc + 1, n +: n +: ns, heap, callStack, result)
              case _ => eose
            }
            case "swap" => stack match{
              case n1 +: n2 +: ns => wsi(pc + 1, n2 +: n1 +: ns, heap, callStack, result)
              case _ => eose
            }
            case "discard" => stack match{
              case _ +: ns => wsi(pc + 1, ns, heap, callStack, result)
              case _ => eose
            }
            case "add" => stack match{
              case n1 +: n2 +: ns => wsi(pc + 1, (n1 + n2) +: ns, heap, callStack, result)
              case _ => eose
            }
            case "subt" => stack match{
              case n1 +: n2 +: ns => wsi(pc + 1, (n1 - n2) +: ns, heap, callStack, result)
              case _ => eose
            }
            case "mult" => stack match{
              case n1 +: n2 +: ns => wsi(pc + 1, (n1 * n2) +: ns, heap, callStack, result)
              case _ => eose
            }
            case "intDiv" => stack match{
              case n1 +: n2 +: ns => wsi(pc + 1, (n1 / n2) +: ns, heap, callStack, result)
              case _ => eose
            }
            case "mod" => stack match{
              case n1 +: n2 +: ns => wsi(pc + 1, (n1 % n2) +: ns, heap, callStack, result)
              case _ => eose
            }
            case "store" => stack match{
              case n1 +: n2 +: ns => wsi(pc + 1, ns, heap.updated(n2, n1), callStack, result)
              case _ => eose
            }
            case "get" => stack match{
              case n +: ns => heap.get(n) match{
                case Some(n2) => wsi(pc + 1, n2 +: ns, heap, callStack, result)
                case None => nihe
              }
              case _ => eose
            }
            case "outChar" => stack match{
              case n +: ns =>
                if(log) print(n.toChar)
                wsi(pc + 1, ns, heap, callStack, result :+ n.toChar)
              case _ => eose
            }
            case "outNum" => stack match{
              case n +: ns =>
                if(log) print(n)
                wsi(pc + 1, ns, heap, callStack, result ++ n.toString)
              case _ => eose
            }
            case "readChar" => wsi(pc + 1, StdIn.readChar.toLong +: stack, heap, callStack, result)
            case "readNum" => wsi(pc + 1, StdIn.readLong +: stack, heap, callStack, result)
            case "call" => callAddrs.get(num) match{
              case Some(n) => wsi(n, stack, heap, (pc + 1) +: callStack, result)
              case None => lnfe
            }
            case "jump" => callAddrs.get(num) match{
              case Some(n) => wsi(n, stack, heap, callStack, result)
              case None => lnfe
            }
            case "jumpZero" => stack match{
              case 0L +: ns =>
                callAddrs.get(num) match{
                  case Some(n) => wsi(n, ns, heap, callStack, result)
                  case None => lnfe
                }
              case _ +: ns => wsi(pc + 1, ns, heap, callStack, result)
              case _ => eose
            }
            case "jumpNeg" => stack match{
              case chk +: ns if chk < 0 =>
                callAddrs.get(num) match{
                  case Some(n) => wsi(n, ns, heap, callStack, result)
                  case None => lnfe
                }
              case _ +: ns => wsi(pc + 1, ns, heap, callStack, result)
              case _ => eose
            }
            case "label" => wsi(pc + 1, stack, heap, callStack, result)
            case "return" => callStack match{
              case a +: as => wsi(a, stack, heap, as, result)
              case _ => snfe
            }
            case "endProg" => Success(result)
          }
      }
    }
    
    wsi(0, List[Long](), immutable.HashMap[Long, Long](), List[Int](), "")
  }
  
  def formatProg(prog: String): Vector[(String, Long)] = {
    val syntax: Vector[(String, String)] = Vector[(String, String)](
      ("  ", "push"),
      (" \n ", "dup"),
      (" \n\t", "swap"),
      (" \n\n", "discard"),
      ("\t   ", "add"),
      ("\t  \t", "subt"),
      ("\t  \n", "mult"),
      ("\t \t ", "intDiv"),
      ("\t \t\t", "mod"),
      ("\t\t ", "store"),
      ("\t\t\t", "get"),
      ("\n  ", "label"),
      ("\n \t", "call"),
      ("\n \n", "jump"),
      ("\n\t ", "jumpZero"),
      ("\n\t\t", "jumpNeg"),
      ("\n\t\n", "return"),
      ("\n\n\n", "endProg"),
      ("\t\n  ", "outChar"),
      ("\t\n \t", "outNum"),
      ("\t\n\t ", "readChar"),
      ("\t\n\t\t", "readNum"))
    val nonArgOps: Vector[String] = Vector[String]("dup", "swap", "discard", "add", "subt", "mult", "intDiv", "mod", "store", "get", "return", "endProg", "outChar", "outNum", "readChar", "readNum")
    val synKeys = syntax.map(_._1).sortWith(_.length > _.length)
    val synMap: immutable.HashMap[String, String] = {
      val builder = immutable.HashMap.newBuilder[String, String]
      builder ++= syntax
      builder.result
    }
    
    def isNotLF(c: Char): Boolean = (c == ' ') || (c == '\t')
    def longNum(str: String): Long = str.takeWhile(isNotLF).reverse.zipWithIndex.map{case (c, i) => if(c == '\t') Math.pow(2, i).toLong else 0L}.sum
    
    @tailrec
    def fHelper(ac: Vector[(String, Long)], src: String): Vector[(String, Long)] = {
      synKeys.find(src.startsWith) match{
        case Some(key) =>
          val tag = synMap(key)
          if(nonArgOps.contains(tag)) fHelper(ac :+ ((tag, 0L)), src.drop(key.length))
          else{
            val tail = src.drop (key.length)
            val lNum = longNum (tail)
            fHelper (ac :+ ((tag, lNum)), tail.dropWhile (isNotLF).tail)
          }
        case None => if(src.nonEmpty) fHelper(ac, src.tail) else ac
      }
    }
    
    val conditioned = prog.replaceAll("(\r\n|\r)", "\n").filter("\t\n ".contains(_))
    fHelper(Vector[(String, Long)](), conditioned)
  }
}