package scala_run

import common.{Config, Interpreter}

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.util.{Failure, Success, Try}

object ScalaRun extends Interpreter{
  val name: String = "Scala"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = builder(progRaw) map {func => {_ => LazyList(exec(func)).flatten}}
  
  def exec(a: Seq[String] => Unit): String = tryAll{a(Seq())} match{
    case Failure(e) => s"Error: $e"
    case Success(_) => ""}
  
  def builder(prog: String): Try[Seq[String] => Unit] = Try{
    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(prog)
    
    import toolbox.u._
    val sym = toolbox.define(tree.asInstanceOf[ImplDef])
    args => toolbox.eval(q"$sym.main(Array(${args.mkString(", ")}))")}
}
