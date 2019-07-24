package brainfuck

import scala.collection.mutable

/** Syntax:
  *
  * [ => *gasp*
  *
  * ] => *pomf*
  *
  * + => pf
  *
  * - => bl
  *
  * > => b
  *
  * < => t
  *
  * . => !
  *
  * , => ?
  */

object FlufflePuff extends BFTranslator{
  val name: String = "FlufflePuff"
  val baseLang: String = "BrainFuck"
  lazy val kvPairs: Vector[(String, String)] = Vector[(String, String)](
    ("[", "*gasp*"),
    ("]", "*pomf*"),
    ("+", "pf"),
    ("-", "bl"),
    (">", "b"),
    ("<", "t"),
    (".", "!"),
    (",", "?"))
  
  def transFrom(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(prog: String): String = vals.foldLeft(prog){(str, key) => str.replaceAllLiterally(key, revSyntax(key))}
  def transTo(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(prog: String): String = keys.foldLeft(prog){(str, key) => str.replaceAllLiterally(key, syntax(key))}
}
