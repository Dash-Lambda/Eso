package interpreters

import Compilers.BFCompiler

import scala.util.{Failure, Success, Try}

object BFCompiled extends Interpreter{
  def apply(log: Boolean, debug: Boolean)(progRaw: String): Try[String] = apply(40000, -1, dynamicTapeSize = false, log = true, debug = false)(progRaw)
  def apply(initTapeSize: Int, outputMaxLength: Int, dynamicTapeSize: Boolean, log: Boolean, debug: Boolean)(progRaw: String): Try[String] = {
    BFCompiler(initTapeSize, outputMaxLength, dynamicTapeSize, log, debug)(progRaw) match {
      case Success(prog) => Try{
        if(debug) print("Compiling... ")
        val interp = InterpFactory.make(prog)
        if(debug) println("Done.")
        val res = interp.apply
        res
      }
      case Failure(e) => Failure(e)
    }
  }
}
