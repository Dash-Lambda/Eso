package assemblers

import scala.util.Try

trait Assembler {
  def apply(prog: Vector[String], log: Boolean): Try[String]
  def unapply(prog: String, log: Boolean): Try[String]
}
