package translators

import scala.annotation.tailrec
import scala.collection.immutable

class GenericBFTranslator(val name: String, val keys: Vector[String], val vals: Vector[String]) extends BFTranslator{
  def apply(prog: String): String = translate(prog, syntax)
  def unapply(prog: String): String = translate(prog, revSyntax)
  
  private def translate(prog: String, syn: immutable.HashMap[String, String]): String = {
    val keys = syn.keys.toVector.sortWith(_.length > _.length)
    
    @tailrec
    def tHelper(log: String, src: String): String = {
      keys.find(key => key == src.take(key.length)) match{
        case Some(h) => tHelper(log ++ syn(h), src.drop(h.length))
        case None if src.nonEmpty => tHelper(log :+ src.head, src.tail)
        case None if src.isEmpty => log
      }
    }
    
    tHelper("", prog)
  }
}

object GenericBFTranslator{
  def apply(name: String, syntax: immutable.HashMap[String, String]): GenericBFTranslator = {
    val kv = syntax.toVector.unzip
    new GenericBFTranslator(name, kv._1, kv._2)
  }
  
  def apply(name: String, pairs: IndexedSeq[(String, String)]): GenericBFTranslator = {
    val kv = pairs.toVector.unzip
    new GenericBFTranslator(name, kv._1, kv._2)
  }
}