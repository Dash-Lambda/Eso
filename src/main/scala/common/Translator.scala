package common

import scala.collection.mutable
import scala.util.Try

trait Translator extends EsoObj{
  def name: String
  def baseLang: String
  
  def apply(bools: mutable.HashMap[String, (Boolean, String)],
            nums: mutable.HashMap[String, (Int, String)])
           (prog: String): Try[String]
  def unapply(bools: mutable.HashMap[String, (Boolean, String)],
              nums: mutable.HashMap[String, (Int, String)])
             (prog: String): Try[String]
  
  override def toString: String = s"$name <=> $baseLang"
}
