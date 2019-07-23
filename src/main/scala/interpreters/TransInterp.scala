package interpreters

import translators.Translator

import scala.collection.mutable
import scala.util.Try

class TransInterp(trans: Translator, interp: Interpreter) extends Interpreter{
  val name: String = trans.name
  
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String] = interp(bools, nums)(trans(bools, nums)(progRaw))
}
object TransInterp{
  def apply(trans: Translator, interp: Interpreter): TransInterp = new TransInterp(trans, interp)
}
