package interpreters

import Compilers.BFCompiler

import scala.util.{Failure, Success, Try}

import reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

object InterpFactory{
  def make(prog: String): () => String = {
    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(prog)
    val compiled = toolbox.compile(tree)
    compiled().asInstanceOf[() => String]
  }
}

object BFCompiled extends Interpreter{
  def apply(log: Boolean, debug: Boolean)(progRaw: String): Try[String] = apply(40000, -1, dynamicTapeSize = false, log = true, debug = false)(progRaw)
  def apply(initTapeSize: Int, outputMaxLength: Int, dynamicTapeSize: Boolean, log: Boolean, debug: Boolean)(progRaw: String): Try[String] = {
    BFCompiler(initTapeSize, outputMaxLength, dynamicTapeSize, log, debug)(progRaw) match {
      case Success(prog) => Try{
        if(debug) println(prog)
        val interp = InterpFactory.make(prog)
        if(!log) print("Compiled... ")
        val res = interp.apply
        res
      }
      case Failure(e) => Failure(e)
    }
  }
}
