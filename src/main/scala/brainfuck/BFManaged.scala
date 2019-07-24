package brainfuck

import common.{Interpreter, InterpreterException}

import scala.collection.mutable
import scala.util.{Failure, Try}

object BFManaged extends Interpreter{
  val name: String = "BrainFuck"
  
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String] = nums.get("BFOpt") match{
    case Some(bfOpt) => bfOpt._1 match{
      case 0 => BFFunctional(bools, nums)(progRaw)
      case 1 => BFOptimized(bools, nums)(progRaw)
      case 2 => BFCompiled(bools, nums)(progRaw)
    }
    case _ => Failure(InterpreterException("Missing Configuration Values"))
  }
}
