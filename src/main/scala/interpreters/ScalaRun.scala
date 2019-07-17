package interpreters

import scala.util.Try

object ScalaRun extends Interpreter{
  val name = "Scala"
  def apply(log: Boolean, debug: Boolean, outputMaxLength: Int)(progRaw: String): Try[String] = Try{
    if(debug) print("Compiling... ")
    val interp = InterpFactory.make(progRaw)
    if(debug) println("Done.")
    if(outputMaxLength == -1) interp.apply
    else interp.apply.take(outputMaxLength)
  }
}
