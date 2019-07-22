package interpreters

import Compilers.BFCompiler

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object BFCompiled extends Interpreter{
  val name = "BFCompiled"
  
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String] = {
    bools.get("debug") match{
      case Some((debug, _)) => BFCompiler(bools, nums)(progRaw) match {
        case Success(prog) => Try{
          if(debug) print("Compiling... ")
          val interp = ScalFactory.make(prog)
          if(debug) println("Done.")
          val res = interp.apply
          res
        }
        case Failure(e) => Failure(e)
      }
      case None => Failure(InterpreterException("Unspecified Runtime Parameters"))
    }
  }
}
