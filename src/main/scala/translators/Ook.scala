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
  final val kvPairs = Vector[(String, String)](
    (".", "Ook! Ook."),
    (",", "Ook. Ook!"),
    (">", "Ook. Ook?"),
    ("<", "Ook? Ook."),
    ("+", "Ook. Ook."),
    ("-", "Ook! Ook!"),
    ("[", "Ook! Ook?"),
    ("]", "Ook? Ook!"))
  
  def apply(prog: String): String = {
    @tailrec
    def tHelper(log: String, src: String): String = vals.find(_ == src.take(9)) match{
      case Some(tok) => tHelper(log ++ revSyntax(tok), src.drop(9))
      case None if src.sizeIs > 9 => tHelper(log :+ src.head, src.tail)
      case None => log ++ src
    }
    
    tHelper("", prog)
  }
  def unapply(prog: String): String = keys.foldLeft(prog){case (str, key) => str.replaceAllLiterally(key, syntax(key))}
}
