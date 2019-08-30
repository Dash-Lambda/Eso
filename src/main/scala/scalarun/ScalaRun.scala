package scalarun

import common.{Config, EsoExcep, Interpreter}

import scala.util.{Failure, Try}

object ScalaRun extends Interpreter{
  val name: String = "Scala"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = ScalaFactory(progRaw) map{func =>
    {inputs =>
      func(inputs)
      LazyList[Char]()
    }
  }
}
