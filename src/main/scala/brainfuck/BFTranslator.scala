package brainfuck

import common.Translator

import scala.collection.mutable
import scala.util.{Success, Try}

trait BFTranslator extends Translator {
  def kvPairs: Vector[(String, String)]
  
  final lazy val keys = kvPairs.map(_._1)
  final lazy val vals = kvPairs.map(_._2)
  
  final lazy val syntax = buildMap(keys, vals)
  final lazy val revSyntax = buildMap(vals, keys)
  
  def apply(bools: mutable.HashMap[String, (Boolean, String)],
            nums: mutable.HashMap[String, (Int, String)])
           (prog: String): Try[String] = {
    if(bools.isDefinedAt("debug") && bools("debug")._1) println(this.toString)
    Success(transFrom(bools, nums)(prog))
  }
  def unapply(bools: mutable.HashMap[String, (Boolean, String)],
              nums: mutable.HashMap[String, (Int, String)])
             (prog: String): Try[String] = {
    if(bools.isDefinedAt("debug") && bools("debug")._1) println(this.toString)
    Success(transTo(bools, nums)(prog))
  }
  
  def transFrom(bools: mutable.HashMap[String, (Boolean, String)],
            nums: mutable.HashMap[String, (Int, String)])
           (prog: String): String
  def transTo(bools: mutable.HashMap[String, (Boolean, String)],
              nums: mutable.HashMap[String, (Int, String)])
             (prog: String): String
}
