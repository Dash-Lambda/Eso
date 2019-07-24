package common

import scala.collection.mutable
import scala.util.Try

trait Interpreter extends EsoObj{
  def name: String
  
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String]
  
  override def toString: String = name
}
