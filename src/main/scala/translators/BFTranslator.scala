package translators

import scala.collection.immutable

trait BFTranslator {
  def name: String
  
  def keys: Vector[String]
  def vals: Vector[String]
  
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