package common

import parsers.EsoParser

import scala.util.Try

abstract class AbstractionEliminator extends Transpiler{
  val src: String = "Lambda_Calculus"
  
  val parser: EsoParser[Seq[Char], Expr]
  
  def apply(config: Config)(progRaw: String): Try[String] = parser(progRaw).toTry() map (_.toString)
  
  abstract class Expr{
    def elim(c: Char): Expr
    def uses(c: Char): Boolean}
}
