package brainfuck

import common.Config

import scala.annotation.tailrec
import scala.util.{Success, Try}

object Ook extends BFTranslator{
  val name: String = "Ook"
  val baseLang: String = "BrainFuck"
  val kvPairs: Vector[(String, String)] = Vector[(String, String)](
    (".", "Ook! Ook."),
    (",", "Ook. Ook!"),
    (">", "Ook. Ook?"),
    ("<", "Ook? Ook."),
    ("+", "Ook. Ook."),
    ("-", "Ook! Ook!"),
    ("[", "Ook! Ook?"),
    ("]", "Ook? Ook!"))
  
  def apply(config: Config)(progRaw: String): Try[String] = {
    @tailrec
    def tHelper(log: String, src: String): String = vals.find(_ == src.take(9)) match{
      case Some(tok) => tHelper(log ++ revSyntax(tok), src.drop(9))
      case None if src.sizeIs > 9 => tHelper(log :+ src.head, src.tail)
      case None => log ++ src
    }
  
    Success(tHelper("", progRaw))
  }
  def unapply(config: Config)(progRaw: String): Try[String] = Success(keys.foldLeft(progRaw){case (str, key) => str.replaceAllLiterally(key, syntax(key))})
}
