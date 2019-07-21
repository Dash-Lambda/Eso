package interpreters

import scala.collection.immutable
import scala.util.Try

trait Interpreter {
  def name: String
  //Nums: outputMaxLength, initTapeSize, BFOpt, dbTim
  //Flags: log, debug, dynamicTapeSize
  def apply(flags: Vector[Boolean], nums: Vector[Int])(progRaw: String): Try[String]
  def mkMap[A, B](vec: Vector[(A, B)]): immutable.HashMap[A, B] = {
    val builder = immutable.HashMap.newBuilder[A, B]
    builder ++= vec
    builder.result
  }
}