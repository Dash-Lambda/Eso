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

object BrainPuff extends BFTranslator{
  final val name = "BrainPuff"
  
  final lazy val keys: Vector[String] = Vector("*gasp*", "*pomf*", "pf", "bl", "b", "t", "!", "?")
  final lazy val vals: Vector[String] = Vector("[", "]", "+", "-", ">", "<", ".", ",")
  
  def apply(prog: String): String = keys.foldLeft(prog){(str, key) => str.replaceAllLiterally(key, syntax(key))}
  def unapply(prog: String): String = revSyntax.keys.foldLeft(prog){(str, key) => str.replaceAllLiterally(key, revSyntax(key))}
}
