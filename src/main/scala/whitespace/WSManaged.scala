package whitespace

import common.{Interpreter, InterpreterException}

import scala.collection.mutable
import scala.util.{Failure, Try}

object WSManaged extends Interpreter{
  val name: String = "WhiteSpace"
  
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String] = bools.get("WSL") match{
    case Some(p) =>
      if(p._1) WhiteSpaceSL(bools, nums)(progRaw)
      else WhiteSpace(bools, nums)(progRaw)
    case None => Failure(InterpreterException("Missing Configuration Parameter: WSL"))
  }
}
