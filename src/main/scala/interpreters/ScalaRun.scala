package interpreters

import scala.util.{Failure, Try}

object ScalaRun extends Interpreter{
  val name = "Scala"
  def apply(flags: Vector[Boolean], nums: Vector[Int])(progRaw: String): Try[String] = (flags, nums) match{
    case (_ +: debug +: _, outputMaxLength +: _) => Try{
      if(debug) print("Compiling... ")
      val interp = InterpFactory.make(progRaw)
      if(debug) println("Done.")
      if(outputMaxLength == -1) interp.apply
      else interp.apply.take(outputMaxLength)
    }
    case _ => Failure(InterpreterException("Missing Config Values"))
  }
}
