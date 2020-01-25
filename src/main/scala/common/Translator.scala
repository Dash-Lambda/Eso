package common

import scala.util.Try

abstract class Translator extends EsoObj{
  val name: String
  val baseLang: String
  
  def id: (String, String) = (name, baseLang)
  def apply(config: Config)(prog: String): Try[String]
  def unapply(config: Config)(prog: String): Try[String]
  override def toString: String = s"$name <=> $baseLang"
}
