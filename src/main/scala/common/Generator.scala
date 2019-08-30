package common

import scala.util.Try

trait Generator {
  val src: String
  val dst: String
  
  def id: (String, String) = (src, dst)
  def apply(config: Config)(progRaw: String): Try[String]
}
