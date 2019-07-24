package scalarun

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

object ScalFactory{
  def make(prog: String): () => String = {
    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(prog)
    val compiled = toolbox.compile(tree)
    compiled().asInstanceOf[() => String]
  }
}
