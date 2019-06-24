package translators

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
  final val name = "FlufflePuff"
  final lazy val kvPairs = Vector[(String, String)](
    ("[", "*gasp*"),
    ("]", "*pomf*"),
    ("+", "pf"),
    ("-", "bl"),
    (">", "b"),
    ("<", "t"),
    (".", "!"),
    (",", "?"))
  
  def apply(prog: String): String = vals.foldLeft(prog){(str, key) => str.replaceAllLiterally(key, revSyntax(key))}
  def unapply(prog: String): String = keys.foldLeft(prog){(str, key) => str.replaceAllLiterally(key, syntax(key))}
}