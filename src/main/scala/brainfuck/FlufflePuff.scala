package brainfuck

import common.{Config, Translator}

import scala.util.{Success, Try}

object FlufflePuff extends BFTranslator{
  val name: String = "FlufflePuff"
  val baseLang: String = "BrainFuck"
  val kvPairs: Vector[(String, String)] = Vector[(String, String)](
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
