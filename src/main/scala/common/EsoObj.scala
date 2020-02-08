package common

import scala.collection.{immutable, mutable}
import scala.util.{Failure, Success, Try}

trait EsoObj {
  def tryAll[T](thing: => T): Try[T] = try{Success(thing)} catch{case e: Throwable => Failure(e)}
  
  def mkMut[A, B](immut: immutable.HashMap[A, B]): mutable.HashMap[A, B] = mutable.HashMap.from(immut)
  def mkImmut[A, B](mut: mutable.HashMap[A, B]): immutable.HashMap[A, B] = immutable.HashMap.from(mut)
  def mkMap(ks: Seq[String], vs: Seq[String]): immutable.HashMap[String, String] = mkMap(ks.zip(vs))
  def mkMap[A, B](vec: Seq[(A, B)]): immutable.HashMap[A, B] = {
    val builder = immutable.HashMap.newBuilder[A, B]
    builder ++= vec
    builder.result}
  
  def filterChars(str: String, cs: Seq[Char]): String = str.filter(cs.contains(_))
  def filterContains[A](seq: Seq[A], cs: Seq[A]): Seq[A] = seq.filter(cs.contains(_))
  
  def StringToRect(str: String): Vector[Vector[Int]] = {
    val lines = str.linesIterator.map(_.toVector.map(_.toInt)).toVector
    val xdim = lines.map(_.size).max
    lines.map(v => v.padTo(xdim, 32))}
}
