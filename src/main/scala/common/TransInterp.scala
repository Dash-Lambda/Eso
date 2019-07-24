package common

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class TransInterp(trans: Translator, interp: Interpreter) extends Interpreter{
  val name: String = trans.name
  
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String] = trans(bools, nums)(progRaw) match{
    case Success(prog) => interp(bools, nums)(prog)
    case Failure(e) => Failure(e)
  }
  
  override def toString: String = s"${trans.name} (${interp.name})"
}
object TransInterp{
  def apply(trans: Translator, interp: Interpreter): TransInterp = new TransInterp(trans, interp)
}
