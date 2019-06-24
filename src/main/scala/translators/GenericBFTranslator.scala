package translators

import scala.annotation.tailrec
import scala.collection.immutable

class GenericBFTranslator(val name: String, val kvPairs: Vector[(String, String)]) extends BFTranslator{
  def apply(prog: String): String = translate(prog, revSyntax)
  def unapply(prog: String): String = translate(prog, syntax)
  
  private def translate(prog: String, syn: immutable.HashMap[String, String]): String = {
    val keysOrder = syn.keys.toVector.sortWith(_.length > _.length)
    
    @tailrec
    def tHelper(log: String, src: String): String = keysOrder.find(src.startsWith) match{
      case Some(k) => tHelper(log ++ syn(k), src.drop(k.length))
      case None if src.nonEmpty => tHelper(log :+ src.head, src.tail)
      case None if src.isEmpty => log
    }
    
    tHelper("", prog)
  }
}

object GenericBFTranslator{
  def apply(name: String, syntax: immutable.HashMap[String, String]): GenericBFTranslator = {
    new GenericBFTranslator(name, syntax.toVector)
  }
  
  def apply(name: String, pairs: IndexedSeq[(String, String)]): GenericBFTranslator = {
    new GenericBFTranslator(name, pairs.toVector)
  }
}