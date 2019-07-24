package whitespace

import common.{Interpreter, InterpreterException}

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object WhiteSpace extends Interpreter {
  val name = "WhiteSpaceL"
  
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String] = {
    getParms(bools, nums)("log", "debug")("outputMaxLength", "dbTim") match{
      case Some((log +: debug +: _, outputMaxLength +: dbTim +: _)) => Try{formatProg(progRaw)} match{
        case Success(prog) => wsFunc(prog, getCalls(prog), log, debug, outputMaxLength, dbTim)
        case Failure(e) => Failure(e)
      }
      case None => Failure(InterpreterException("Unspecified Runtime Parameters"))
    }
  }
  
  def wsFunc(prog: Vector[(String, Long)], callAddrs: immutable.HashMap[Long, Int], log: Boolean, debug: Boolean, outputMaxLength: Int, dbTim: Int): Try[String] = {
    def binOp(func: (Long, Long) => Long)(stack: List[Long]): List[Long] = stack match{case n1 +: n2 +: ns => func(n1, n2) +: ns}
    def printWrap(tok: String): String = {if(log && !debug) print(tok); tok}
    
    @tailrec
    def wsi(pc: Int, stack: List[Long], heap: immutable.HashMap[Long, Long], callStack: List[Int], result: String): String = {
      val (op, num) = prog(pc)
      if(debug){
        println(s"WS: $op $num (${stack.mkString(" | ")}) [${heap.mkString(" | ")}]")
        Thread.sleep(dbTim)
      }
      op match{
        case "push" => wsi(pc + 1, num +: stack, heap, callStack, result)
        case "dup" => stack match{case n +: ns => wsi(pc + 1, n +: n +: ns, heap, callStack, result)}
        case "swap" => stack match{case n1 +: n2 +: ns => wsi(pc + 1, n2 +: n1 +: ns, heap, callStack, result)}
        case "discard" => stack match{case _ +: ns => wsi(pc + 1, ns, heap, callStack, result)}
        case "add" => wsi(pc + 1, binOp(_+_)(stack), heap, callStack, result)
        case "subt" => wsi(pc + 1, binOp(_-_)(stack), heap, callStack, result)
        case "mult" => wsi(pc + 1, binOp(_*_)(stack), heap, callStack, result)
        case "intDiv" => wsi(pc + 1, binOp(_/_)(stack), heap, callStack, result)
        case "mod" => wsi(pc + 1, binOp(_%_)(stack), heap, callStack, result)
        case "store" => stack match{case n1 +: n2 +: ns => wsi(pc + 1, ns, heap.updated(n2, n1), callStack, result)}
        case "get" => stack match{case n +: ns => wsi(pc + 1, heap(n) +: ns, heap, callStack, result)}
        case "label" => wsi(pc + 1, stack, heap, callStack, result)
        case "call" => wsi(callAddrs(num), stack, heap, (pc + 1) +: callStack, result)
        case "jump" => wsi(callAddrs(num), stack, heap, callStack, result)
        case "jumpZero" => if(stack.head == 0) wsi(callAddrs(num), stack.tail, heap, callStack, result) else wsi(pc + 1, stack.tail, heap, callStack, result)
        case "jumpNeg" => if(stack.head < 0) wsi(callAddrs(num), stack.tail, heap, callStack, result) else wsi(pc + 1, stack.tail, heap, callStack, result)
        case "return" => callStack match{case a +: as => wsi(a, stack, heap, as, result)}
        case "endProg" => result
        case "readChar" => wsi(pc + 1, StdIn.readChar.toLong +: stack, heap, callStack, result)
        case "readNum" => wsi(pc + 1, StdIn.readLong +: stack, heap, callStack, result)
        case "outChar" => stack match{case n +: ns =>
          if(outputMaxLength == -1) wsi(pc + 1, ns, heap, callStack, result ++ printWrap(n.toChar.toString))
          else{
            val nxt = result ++ printWrap(n.toChar.toString)
            if(nxt.sizeIs < outputMaxLength) wsi(pc + 1, ns, heap, callStack, nxt)
            else nxt
          }
        }
        case "outNum" => stack match{case n +: ns =>
          if(outputMaxLength == -1) wsi(pc + 1, ns, heap, callStack, result ++ printWrap(n.toString))
          else{
            val nxt = result ++ printWrap(n.toString)
            if(nxt.sizeIs < outputMaxLength) wsi(pc + 1, ns, heap, callStack, nxt)
            else nxt
          }
        }
      }
    }
    
    Try{wsi(0, List[Long](), immutable.HashMap[Long, Long](), List[Int](), "")}
  }
  
  def getCalls(vec: Vector[(String, Long)]): immutable.HashMap[Long, Int] = mkMap(vec.zipWithIndex.filter{case ((op, _), _) => op == "label"}.map{case ((_, num), ind) => (num, ind)})
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
    val synMap: immutable.HashMap[String, String] = mkMap(syntax)
    
    def longNum(str: String): Long = {
      val signum = str.head match{case ' ' => 1; case '\t' => -1}
      signum*str.tail.takeWhile(_ != '\n').reverse.zipWithIndex.map{case (c, i) => if(c == '\t') Math.pow(2, i).toLong else 0L}.sum
    }
    
    @tailrec
    def fHelper(ac: Vector[(String, Long)], src: String): Vector[(String, Long)] = synKeys.find(src.startsWith) match{
      case Some(key) =>
        val tag = synMap(key)
        if(nonArgOps.contains(tag)) fHelper(ac :+ ((tag, 0L)), src.drop(key.length))
        else{
          val tail = src.drop (key.length)
          val lNum = longNum (tail)
          fHelper (ac :+ ((tag, lNum)), tail.dropWhile (_ != '\n').tail)
        }
      case None => if(src.nonEmpty) fHelper(ac, src.tail) else ac
    }
    
    val conditioned = prog.replaceAll("(\r\n|\r)", "\n").filter("\t\n ".contains(_))
    fHelper(Vector[(String, Long)](), conditioned)
  }
}
