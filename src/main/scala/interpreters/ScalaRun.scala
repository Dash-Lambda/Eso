package interpreters

import scala.util.Try

object ScalaRun extends Interpreter{
  def apply(log: Boolean, debug: Boolean)(progRaw: String): Try[String] = Try{
    if(debug) print("Compiling... ")
    val interp = InterpFactory.make(progRaw)
    if(debug) println("Done.")
    interp.apply
  }
}
