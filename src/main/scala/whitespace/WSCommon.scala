package whitespace

import common.{OrderedChunkParser, EsoObj, OrderedParser}
import spire.math.SafeLong
import spire.implicits._

import scala.collection.immutable

object WSCommon extends EsoObj {
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
  val synMap: immutable.HashMap[String, (String, String)] = mkMap(syntax.map { case (k, v) => (k, (k, v)) })
  val revMap: immutable.HashMap[String, String] = mkMap(syntax.map { case (k, v) => (v, k) })
  
  val argOpParser: OrderedParser[String, (String, SafeLong)] = {
    def longNum(str: String): SafeLong = {
      val signum = str.head match {
        case ' ' => SafeLong(1)
        case '\t' => SafeLong(-1)}
      val mag = str
        .tail
        .takeWhile(_ != '\n')
        .reverse
        .zipWithIndex
        .map { case (c, i) => if (c == '\t') SafeLong(2).pow(i) else SafeLong(0) }
        .foldLeft(SafeLong(0)) { case (a, b) => a + b }
      signum * mag}
    
    val argMaps: Vector[(String, String)] = Vector(
      ("  ", "push"),
      ("\n  ", "label"),
      ("\n \t", "call"),
      ("\n \n", "jump"),
      ("\n\t ", "jumpZero"),
      ("\n\t\t", "jumpNeg"))
    OrderedChunkParser{inp =>
      argMaps.find(p => inp.startsWith(p._1)) map {
        case (k, v) =>
          val tail = inp.drop(k.length)
          val lNum = longNum(tail)
          ((v, lNum), tail.dropWhile(_ != '\n').tail, 0)}}}
  
  val nonArgParser: OrderedParser[String, (String, SafeLong)] = {
    val maps = Vector(
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
      ("\n\t\n", "return"),
      ("\n\n\n", "endProg"),
      ("\t\n  ", "outChar"),
      ("\t\n \t", "outNum"),
      ("\t\n\t ", "readChar"),
      ("\t\n\t\t", "readNum"))
    OrderedChunkParser{inp =>
      maps.find(p => inp.startsWith(p._1)) map {
        case (k, v) => ((v, SafeLong(0)), inp.drop(k.length), 0)}}}
  
  val wsParser: OrderedParser[String, (String, SafeLong)] = argOpParser <+> nonArgParser
  
  def binNum(num: SafeLong): String = {
    val digs = LazyList
      .unfold(num.abs) { n => if (n == 0) None else Some((n % 2 == 1, n / 2)) }
      .reverse
      .map(if (_) '\t' else ' ')
      .mkString
    s"${if (num < 0) '\t' else ' '}$digs\n"}
  
  def getCalls(prog: Vector[(String, SafeLong)]): immutable.HashMap[SafeLong, Int] = mkMap(prog.zipWithIndex.collect { case (("label", id), n) => (id, n + 1) })
  def parse(progRaw: String): Vector[(String, SafeLong)] = wsParser.parseAllValues(filterChars(progRaw, "\t\n "))
}