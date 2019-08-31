package brainfuck

import common.{Config, EsoExcep, Interpreter}
import scalarun.{ScalaFactory, ScalaRun}

import scala.util.{Failure, Try}

object BFComp extends Interpreter{
  val name: String = "BFComp"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = BFGen(config)(progRaw) flatMap ScalaRun(config)
}
