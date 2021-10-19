package languages.brainfuck

import common.Config

import scala.util.{Success, Try}

object FlufflePuff extends BFTranslator{
  val name: String = "FlufflePuff"
  val baseLang: String = "BrainFuck"
  val kvPairs: Vector[(String, String)] = Vector(
    ("[", "*gasp*"),
    ("]", "*pomf*"),
    ("+", "pf"),
    ("-", "bl"),
    (">", "b"),
    ("<", "t"),
    (".", "!"),
    (",", "?"))
  
  def apply(config: Config)(prog: String): Try[String] = Success(vals.foldLeft(prog){(str, key) => str.replaceAllLiterally(key, revSyntax(key))})
  def unapply(config: Config)(prog: String): Try[String] = Success(keys.foldLeft(prog){(str, key) => str.replaceAllLiterally(key, syntax(key))})
}
