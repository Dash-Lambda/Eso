package assemblers

import scala.collection.immutable
import scala.util.Try

trait Assembler {
  def apply(prog: Vector[String], log: Boolean): Try[String]
  def unapply(prog: String, log: Boolean): Try[String]
  
  def mkMap[A, B](vec: Vector[(A, B)]): immutable.HashMap[A, B] = {
    val builder = immutable.HashMap.newBuilder[A, B]
    builder ++= vec
    builder.result
  }
}