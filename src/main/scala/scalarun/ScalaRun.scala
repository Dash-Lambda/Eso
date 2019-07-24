package scalarun

import common.{Interpreter, InterpreterException}

import scala.collection.mutable
import scala.util.{Failure, Try}

object ScalaRun extends Interpreter{
  val name = "Scala"
  
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String] = {
    getParms(bools, nums)("debug")("outputMaxLength") match{
      case Some((debug +: _, outputMaxLength +: _)) => Try{
        if(debug) print("Compiling... ")
        val interp = ScalFactory.make(progRaw)
        if(debug) println("Done.")
        if(outputMaxLength == -1) interp.apply
        else interp.apply.take(outputMaxLength)
      }
      case None => Failure(InterpreterException("Unspecified Runtime Parameters"))
    }
  }
}
