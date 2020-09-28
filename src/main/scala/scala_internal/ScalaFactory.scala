package scala_internal

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.util.Try

object ScalaFactory extends (String => Try[Seq[Char] => LazyList[Char]]){
  def apply(prog: String): Try[Seq[Char] => LazyList[Char]] = compileSource(prog) flatMap (c => Try(c().asInstanceOf[Seq[Char] => LazyList[Char]]))
  
  def compileSource(prog: String): Try[() => Any] = Try{
    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(prog)
    val compiled = toolbox.compile(tree)
    compiled}
}
