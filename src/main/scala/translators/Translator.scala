package translators

import scala.collection.immutable

trait Translator {
  def apply(prog: String): String
  def unapply(prog: String): String
  
  def mkMap[A, B](vec: Vector[(A, B)]): immutable.HashMap[A, B] = {
    val builder = immutable.HashMap.newBuilder[A, B]
    builder ++= vec
    builder.result
  }
}