package languages.brainfuck

import common.{Config, Interpreter}
import languages.scala_internal.ScalaFactory

import scala.util.Try

object BFComp extends Interpreter{
  val name: String = "BFComp"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    BFToScalaIntern(config)(progRaw) flatMap ScalaFactory.compileSource map{
      cmp =>
        inp => {
          val prog = cmp().asInstanceOf[Seq[Char] => LazyList[Char]]
          prog(inp)}}}
}
