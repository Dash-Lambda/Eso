package brainfuck

import common.Translator

trait BFTranslator extends Translator{
  val kvPairs: Vector[(String, String)]
  
  final lazy val keys = kvPairs.map(_._1)
  final lazy val vals = kvPairs.map(_._2)
  
  final lazy val syntax = mkMap(keys, vals)
  final lazy val revSyntax = mkMap(vals, keys)
}
