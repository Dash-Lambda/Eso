package scala_run

import common.{Config, Interpreter}

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.util.{Failure, Success, Try}

object ScalaRun extends Interpreter{
  val name: String = "Scala"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = builder(progRaw) map {func => {_ => LazyList(exec(func)).flatten}}
  
  def exec(func: () => Any): String = {
    tryAll{func()} match{
      case Failure(e) => s"Error: $e"
      case Success(_) => ""
    }
  }
  
  def builder(prog: String): Try[() => Any] = Try{
    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(prog)
    toolbox.compile(tree)
  }
}
