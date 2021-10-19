package languages.brainfuck

import common.Translator
import org.typelevel.jawn.ast.{JObject, JString}

trait BFTranslator extends Translator{
  val kvPairs: Vector[(String, String)]
  
  final lazy val keys = kvPairs.map(_._1)
  final lazy val vals = kvPairs.map(_._2)
  
  final lazy val syntax = mkMap(keys, vals)
  final lazy val revSyntax = mkMap(vals, keys)
  
  def toJObject: JObject = JObject.fromSeq(kvPairs.map{case (k, v) => (k, JString(v))})
}
