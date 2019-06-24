package translators

import scala.collection.immutable

trait BFTranslator extends Translator {
  def name: String
  def kvPairs: Vector[(String, String)]
  
  final lazy val keys = kvPairs.map(_._1)
  final lazy val vals = kvPairs.map(_._2)
  
  final lazy val syntax = buildMap(keys, vals)
  final lazy val revSyntax = buildMap(vals, keys)
  
  protected final def buildMap(ks: Vector[String], vs: Vector[String]): immutable.HashMap[String, String] = mkMap(ks.zip(vs))
}