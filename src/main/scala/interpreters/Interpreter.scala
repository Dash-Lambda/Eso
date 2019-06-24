package interpreters

import scala.collection.immutable
import scala.util.Try

trait Interpreter {
  def apply(log: Boolean, debug: Boolean)(prog: String): Try[String]
  def mkMap[A, B](vec: Vector[(A, B)]): immutable.HashMap[A, B] = {
    val builder = immutable.HashMap.newBuilder[A, B]
    builder ++= vec
    builder.result
  }
}