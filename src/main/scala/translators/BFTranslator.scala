package translators

import scala.collection.immutable

trait BFTranslator {
  def name: String
  def kvPairs: Vector[(String, String)]
  
  final lazy val keys = kvPairs.map(_._1)
  final lazy val vals = kvPairs.map(_._2)
  
  final lazy val syntax = buildMap(keys, vals)
  final lazy val revSyntax = buildMap(vals, keys)
  
  protected final lazy val builder = immutable.HashMap.newBuilder[String, String]
  protected final def buildMap(ks: Vector[String], vs: Vector[String]): immutable.HashMap[String, String] = {
    builder ++= ks.zip(vs)
    val res = builder.result
    builder.clear
    res
  }
  
  def apply(prog: String): String
  def unapply(prog: String): String
}