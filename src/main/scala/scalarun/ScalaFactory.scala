package scalarun

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.util.Try

object ScalaFactory extends (String => Try[Seq[Char] => String]){
  def apply(prog: String): Try[Seq[Char] => String] = Try{
    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(prog)
    val compiled = toolbox.compile(tree)
    compiled().asInstanceOf[Seq[Char] => String]
  }
}
