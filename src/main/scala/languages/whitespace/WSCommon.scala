package languages.whitespace

import common.EsoObj
import parsers.EsoParser
import parsers.EsoParser._
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
  val synMap: immutable.HashMap[String, (String, String)] = mkMap(syntax.map { case (k, v) => (k, (k, v)) })
  val revMap: immutable.HashMap[String, String] = mkMap(syntax.map { case (k, v) => (v, k) })
  val argOps: Vector[String] = Vector("push", "label", "call", "jump", "jumpZero", "jumpNeg").sortWith(_.length > _.length)
  val nonArgOps: Vector[String] = synVals.diff(argOps)
  val argMaps: Vector[(String, String)] = syntax.filter{case (_, b) => argOps.contains(b)}
  val nonArgMaps: Vector[(String, String)] = syntax.filter{case (_, b) => nonArgOps.contains(b)}
  
  def mappingParse(maps: Vector[(String, String)]): EsoParser[String] = maps.map{case (f, t) => S(f) ^^^ t}.reduce[EsoParser[String]]{case (a, b) => a | b}
  val digitsParser: EsoParser[SafeLong] = (R("^[ \t]+".r) <& S("\n")) map {_
    .toVector
    .reverse
    .zipWithIndex
    .map{case (c, i) => if (c == '\t') SafeLong(2).pow(i) else SafeLong(0)}
    .foldLeft(SafeLong(0)){case (a, b) => a + b}}
  val numParse: EsoParser[SafeLong] = ((S(" ") | S("\t")) <&> digitsParser) map {
    case (" ", n) => n
    case ("\t", n) => -n}
  def nonArgParse: EsoParser[(String, SafeLong)] = mappingParse(nonArgMaps) map (s => (s, SafeLong(0)))
  def argOpParse: EsoParser[(String, SafeLong)] = mappingParse(argMaps) <&> numParse
  def wsParse: EsoParser[(String, SafeLong)] = argOpParse | nonArgParse
  
  def binNum(num: SafeLong): String = {
    val digs = LazyList
      .unfold(num.abs) { n => if (n == 0) None else Some((n % 2 == 1, n / 2)) }
      .reverse
      .map(if (_) '\t' else ' ')
      .mkString
    s"${if (num < 0) '\t' else ' '}$digs\n"}
  
  def getCalls(prog: Vector[(String, SafeLong)]): immutable.HashMap[SafeLong, Int] = mkMap(prog.zipWithIndex.collect { case (("label", id), n) => (id, n + 1) })
  def parse(progRaw: String): Vector[(String, SafeLong)] = wsParse.*(filterChars(progRaw, "\t\n ")).mapped(_._1).getOrElse(Vector())
}