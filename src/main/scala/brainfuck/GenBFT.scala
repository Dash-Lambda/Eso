package brainfuck

import common.Config
import org.typelevel.jawn.ast.JValue

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.{Success, Try}

class GenBFT(val name: String, val kvPairs: Vector[(String, String)]) extends BFTranslator{
  val baseLang: String = "BrainFuck"
  def apply(config: Config)(progRaw: String): Try[String] = Success(translate(progRaw, revSyntax))
  def unapply(config: Config)(progRaw: String): Try[String] = Success(translate(progRaw, syntax))
  
  def translate(prog: String, syn: immutable.HashMap[String, String]): String = {
    val keysOrder = syn.keys.toVector.sortWith(_.length > _.length)
    @tailrec
    def tHelper(log: String, src: String): String = keysOrder.find(src.startsWith) match{
      case Some(k) => tHelper(log ++ syn(k), src.drop(k.length))
      case None if src.nonEmpty => tHelper(log :+ src.head, src.tail)
      case None if src.isEmpty => log}
    tHelper("", prog)}
}
object GenBFT{
  def apply(name: String, syntax: immutable.HashMap[String, String]): GenBFT = {
    new GenBFT(name, syntax.toVector)}
  
  def apply(name: String, pairs: IndexedSeq[(String, String)]): GenBFT = {
    new GenBFT(name, pairs.toVector)}
  
  def apply(name: String, jVal: JValue): GenBFT = {
    new GenBFT(name, "[]<>+-,.".toVector.map(c => (c.toString, jVal.get(c.toString).asString)))}
}
