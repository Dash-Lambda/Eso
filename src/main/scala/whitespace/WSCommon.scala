package whitespace

import common.EsoObj
import spire.math.SafeLong
import spire.implicits._

import scala.annotation.tailrec
import scala.collection.immutable

object WSCommon extends EsoObj{
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
  val synKeys: Vector[String] = syntax.map(_._1).sortWith(_.length > _.length)
  val synVals: Vector[String] = syntax.map(_._2).sortWith(_.length > _.length)
  val argOps: Vector[String] = Vector("push", "label", "call", "jump", "jumpZero", "jumpNeg").sortWith(_.length > _.length)
  val nonArgOps: Vector[String] = synVals.diff(argOps)
  val synMap: immutable.HashMap[String, (String, String)] = mkMap(syntax.map{case (k, v) => (k, (k, v))})
  val revMap: immutable.HashMap[String, String] = mkMap(syntax.map{case (k, v) => (v, k)})
  
  def binNum(num: SafeLong): String = {
    val digs = LazyList
      .unfold(num.abs){n => if(n == 0) None else Some((n%2 == 1, n/2))}
      .reverse
      .map(if(_) '\t' else ' ')
      .mkString
    s"${if(num < 0) '\t' else ' '}$digs\n"
  }
  
  def longNum(str: String): SafeLong = {
    val signum = str.head match{
      case ' ' => SafeLong(1)
      case '\t' => SafeLong(-1)
    }
    val mag = str
      .tail
      .takeWhile(_ != '\n')
      .reverse
      .zipWithIndex
      .map{case (c, i) => if(c == '\t') SafeLong(2).pow(i) else SafeLong(0)}
      .foldLeft(SafeLong(0)){case (a, b) => a + b}
    signum*mag
  }
  
  def getCalls(prog: Vector[(String, SafeLong)]): immutable.HashMap[SafeLong, Int] = mkMap(prog.zipWithIndex.collect{case (("label", id), n) => (id, n + 1)})
  def condition(progRaw: String): Vector[(String, SafeLong)] = {
    @tailrec
    def cdo(ac: Vector[(String, SafeLong)], src: String): Vector[(String, SafeLong)] = synKeys.find(src.startsWith) map synMap match{
      case Some((k, v)) if argOps.contains(v) =>
        val tail = src.drop(k.length)
        val lNum = longNum(tail)
        cdo(ac :+ ((v, lNum)), tail.dropWhile(_ != '\n').tail)
      case Some((k, v)) => cdo(ac :+ ((v, SafeLong(0))), src.drop(k.length))
      case None => if(src.nonEmpty) cdo(ac, src.tail) else ac
    }
    
    cdo(Vector[(String, SafeLong)](), progRaw.replaceAll("(\r\n|\r)", "\n").filter("\t\n ".contains(_)))
  }
}
