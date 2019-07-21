package assemblers

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.{Failure, Success, Try}

object WhiteSpaceAssembler extends Assembler {
  val name = "WhiteSpace"
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
  val synMap: immutable.HashMap[String, String] = mkMap(syntax.map(p => (p._2, p._1)))
  val revSynMap: immutable.HashMap[String, String] = mkMap(syntax)
  val argOps: Vector[String] = Vector[String]("push", "label", "call", "jump", "jumpZero", "jumpNeg").sortWith(_.length > _.length)
  val nonArgOps: Vector[String] = Vector[String]("dup", "swap", "discard", "add", "subt", "mult", "intDiv", "mod", "store", "get", "return", "endProg", "outChar", "outNum", "readChar", "readNum")
  val logMap: immutable.HashMap[Char, Char] = immutable.HashMap[Char, Char]((' ', 'S'), ('\t', 'T'), ('\n', 'L'))
  
  def longToBin(num: Long): String = {
    @tailrec
    def bHelper(ac: Vector[Boolean], src: Long): Vector[Boolean] = src match{
      case 0 => if(ac.nonEmpty) ac else Vector(false)
      case _ => bHelper((src%2 == 1) +: ac, src/2)
    }
    
    val numStr = bHelper(Vector[Boolean](), num.abs).map(b => if(b) '\t' else ' ').mkString
    (if(num < 0) '\t' else ' ') +: numStr :+ '\n'
  }
  
  def apply(prog: Vector[String], log: Boolean): Try[String] = {
    def printLog(str: String): Unit = if(log) println(str)
    
    @tailrec
    def aHelper(ac: String, src: Vector[String]): Try[String] = src match{
      case op +: ops =>
        if(log) print(s"$op")
        op match{
          case tok if tok.startsWith("//") =>
            printLog("")
            aHelper(ac, ops)
          case tok if nonArgOps.contains(tok) =>
            printLog(s" -> ${synMap(tok).map(logMap)}")
            aHelper(ac ++ synMap(tok), ops)
          case tok => argOps.find(tok.startsWith) match{
            case Some(key) =>
              if(tok.exists(_.isDigit)){
                val assembled = synMap(key) ++ longToBin(tok.drop(key.length).toLong)
                printLog(s" -> ${assembled.map(logMap)}")
                aHelper(ac ++ assembled, ops)
              }
              else Failure(AssemblerException("Not Enough Arguments"))
            case None => Failure(AssemblerException("Operation Not Recognized"))
          }
        }
      case _ => Success(ac)
    }
    
    val conditioned = prog.map(_.replaceAll("(\t|\r|\n| )", "")).filter(_.nonEmpty)
    aHelper("", conditioned)
  }
  
  def unapply(prog: String, log: Boolean): Try[String] = {
    val synKeys = syntax.map(_._1).sortWith(_.length > _.length)
    
    def longNum(str: String): Long = {
      val cond = str.takeWhile(_ != '\n')
      val num = cond
        .tail
        .reverse
        .zipWithIndex
        .map{case (c, i) => if(c == '\t') Math.pow(2, i).toLong else 0L}
        .sum
      if(cond.head == ' ') num
      else -num
    }
    
    @tailrec
    def uHelper(ac: Vector[String], src: String): Try[String] = synKeys.find(key => src.startsWith(key)) match{
      case Some(key) =>
        val tag = revSynMap(key)
        if(nonArgOps.contains(tag)) uHelper(ac :+ tag, src.drop(key.length))
        else{
          val tail = src.drop(key.length)
          val lNum = longNum(tail)
          uHelper(ac :+ s"$tag $lNum", tail.dropWhile(_ != '\n').tail)
        }
      case None => if(src.nonEmpty) uHelper(ac, src.tail) else Success(ac.mkString("\n"))
    }
    
    val conditioned = prog.replaceAll("(\r\n|\r)", "\n").filter("\t\n ".contains(_))
    uHelper(Vector[String](), conditioned)
  }
}
