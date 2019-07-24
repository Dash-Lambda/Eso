package brainfuck

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

class GenericBFTranslator(val name: String, val baseLang: String, val kvPairs: Vector[(String, String)]) extends BFTranslator{
  def transFrom(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(prog: String): String = translate(prog, revSyntax, bools.get("debug"))
  def transTo(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(prog: String): String = translate(prog, syntax, bools.get("debug"))
  
  private def translate(prog: String, syn: immutable.HashMap[String, String], dop: Option[(Boolean, String)]): String = {
    val keysOrder = syn.keys.toVector.sortWith(_.length > _.length)
    val debug = dop match{
      case Some(p) => p._1
      case None => false
    }
    
    @tailrec
    def tHelper(log: String, src: String): String = keysOrder.find(src.startsWith) match{
      case Some(k) =>
        if(debug) print(s"[${syn(k)}]")
        tHelper(log ++ syn(k), src.drop(k.length))
      case None if src.nonEmpty =>
        if(debug) print(src.head)
        tHelper(log :+ src.head, src.tail)
      case None if src.isEmpty =>
        if(debug) println
        log
    }
    
    tHelper("", prog)
  }
}

object GenericBFTranslator{
  def apply(name: String, syntax: immutable.HashMap[String, String]): GenericBFTranslator = {
    new GenericBFTranslator(name, "BrainFuck", syntax.toVector)
  }
  
  def apply(name: String, pairs: IndexedSeq[(String, String)]): GenericBFTranslator = {
    new GenericBFTranslator(name, "BrainFuck", pairs.toVector)
  }
}