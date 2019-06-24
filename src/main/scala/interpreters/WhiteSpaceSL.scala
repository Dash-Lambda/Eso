package interpreters

import spire.math.SafeLong
import spire.implicits._

import scala.annotation.tailrec
import scala.collection.immutable
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object WhiteSpaceSL extends Interpreter {
  def apply(log: Boolean, debug: Boolean)(progRaw: String): Try[String] = Try{formatProg(progRaw)} match{
    case Success(prog) => apply(prog, getCalls(prog), log, debug)
    case Failure(e) => Failure(e)
  }
  
  def apply(prog: Vector[(String, SafeLong)], callAddrs: immutable.HashMap[SafeLong, Int], log: Boolean, debug: Boolean): Try[String] = {
    def binOp(func: (SafeLong, SafeLong) => SafeLong)(stack: List[SafeLong]): List[SafeLong] = stack match{case n1 +: n2 +: ns => func(n1, n2) +: ns}
    def printWrap(tok: String): String = {if(log) print(tok); tok}
    
    @tailrec
    def wsi(pc: Int, stack: List[SafeLong], heap: immutable.HashMap[SafeLong, SafeLong], callStack: List[Int], result: String): String = {
      val (op, num) = prog(pc)
      if(debug) println(s"WS: $op $num (${stack.mkString(" | ")}) [${heap.mkString(" | ")}]")
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
        case "outChar" => stack match{case n +: ns => wsi(pc + 1, ns, heap, callStack, result ++ printWrap(n.toChar.toString))}
        case "outNum" => stack match{case n +: ns => wsi(pc + 1, ns, heap, callStack, result ++ printWrap(n.toString))}
        case "readChar" => wsi(pc + 1, StdIn.readChar.toLong +: stack, heap, callStack, result)
        case "readNum" => wsi(pc + 1, StdIn.readLong +: stack, heap, callStack, result)
      }
    }
    
    Try{wsi(0, List[SafeLong](), immutable.HashMap[SafeLong, SafeLong](), List[Int](), "")}
  }
  
  def getCalls(vec: Vector[(String, SafeLong)]): immutable.HashMap[SafeLong, Int] = mkMap(vec.zipWithIndex.filter{case ((op, _), _) => op == "label"}.map{case ((_, num), ind) => (num, ind)})
  def formatProg(prog: String): Vector[(String, SafeLong)] = {
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
    
    def longNum(str: String): SafeLong = {
      val signum = str.head match{case ' ' => SafeLong(1); case '\t' => SafeLong(-1)}
      signum*str.tail.takeWhile(_ != '\n').reverse.zipWithIndex.map{case (c: Char, i: Int) => if(c == '\t') SafeLong(2).pow(i) else SafeLong(0)}.foldLeft(SafeLong(0)){case (a: SafeLong, b: SafeLong) => a + b}
    }
    
    @tailrec
    def fHelper(ac: Vector[(String, SafeLong)], src: String): Vector[(String, SafeLong)] = synKeys.find(src.startsWith) match{
      case Some(key) =>
        val tag = synMap(key)
        if(nonArgOps.contains(tag)) fHelper(ac :+ ((tag, SafeLong(0))), src.drop(key.length))
        else{
          val tail = src.drop (key.length)
          val lNum = longNum (tail)
          fHelper (ac :+ ((tag, lNum)), tail.dropWhile (_ != '\n').tail)
        }
      case None => if(src.nonEmpty) fHelper(ac, src.tail) else ac
    }
    
    val conditioned = prog.replaceAll("(\r\n|\r)", "\n").filter("\t\n ".contains(_))
    fHelper(Vector[(String, SafeLong)](), conditioned)
  }
}