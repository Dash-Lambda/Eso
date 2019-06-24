package interpreters

import scala.util.Try

trait Interpreter {
  def apply(log: Boolean)(prog: String): Try[String]
}
