package common

import scala.util.Try

trait Interpreter extends EsoObj{
  val name: String
  override def toString: String = name
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]]
}
