package scala_internal

import java.util.concurrent.BlockingQueue

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.util.Try

object ScalaFactory extends (String => Try[(BlockingQueue[Option[Try[Char]]], Seq[Char]) => Runnable]){
  def apply(prog: String): Try[(BlockingQueue[Option[Try[Char]]], Seq[Char]) => Runnable] = Try{
    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(prog)
    val compiled = toolbox.compile(tree)
    compiled().asInstanceOf[(BlockingQueue[Option[Try[Char]]], Seq[Char]) => Runnable]
  }
}
