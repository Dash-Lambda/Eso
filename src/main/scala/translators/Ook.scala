package translators

import scala.annotation.tailrec

/** Syntax:
  *
  * [ => Ook! Ook?
  *
  * ] => Ook? Ook!
  *
  * + => Ook. Ook.
  *
  * - => Ook! Ook!
  *
  * > => Ook. Ook?
  *
  * < => Ook? Ook.
  *
  * . => Ook! Ook.
  *
  * , => Ook. Ook!
  */

object Ook extends BFTranslator{
  final val name = "Ook"
  
  final lazy val keys: Vector[String] = Vector("Ook! Ook.", "Ook. Ook!", "Ook. Ook?", "Ook? Ook.", "Ook. Ook.", "Ook! Ook!", "Ook! Ook?", "Ook? Ook!")
  final lazy val vals: Vector[String] = Vector(".", ",", ">", "<", "+", "-", "[", "]")
  
  def apply(prog: String): String = {
    @tailrec
    def tHelper(log: String, src: String): String = keys.find(_ == src.take(9)) match{
      case Some(key) => tHelper(log ++ syntax(key), src.drop(9))
      case None if src.sizeIs > 9 => tHelper(log :+ src.head, src.tail)
      case None => log ++ src
    }
    
    tHelper("", prog)
  }
  def unapply(prog: String): String = vals.foldLeft(prog){case (str, key) => str.replaceAllLiterally(key, revSyntax(key))}
}
