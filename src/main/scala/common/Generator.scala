package common

import scala.collection.mutable
import scala.util.Try

trait Generator extends EsoObj{
  def name: (String, String) = (src, dst)
  def src: String
  def dst: String
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String]
}
