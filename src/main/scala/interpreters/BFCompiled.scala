package interpreters

import Compilers.BFCompiler

import scala.util.{Failure, Success, Try}

object BFCompiled extends Interpreter{
  val name = "BFCompiled"
  def apply(flags: Vector[Boolean], nums: Vector[Int])(progRaw: String): Try[String] = (flags, nums) match{
    case (log +: debug +: dynamicTapeSize +: _, outputMaxLength +: initTapeSize +: _) => apply(initTapeSize, outputMaxLength, dynamicTapeSize, log, debug)(progRaw)
    case _ => Failure(InterpreterException("Missing Configuration Values"))
  }
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
