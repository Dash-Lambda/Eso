package Compilers

import scala.util.Try

trait Compiler {
  def name: String
  def apply(debug: Boolean)(progRaw: String): Try[String]
}
