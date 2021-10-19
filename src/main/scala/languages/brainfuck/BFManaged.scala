package languages.brainfuck

import common.{Config, EsoExcep, Interpreter}

import scala.util.{Failure, Try}

object BFManaged extends Interpreter{
  val name: String = "BrainFuck"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    Try{config.num("bfOpt")} flatMap{
      case 0 => BFBase(config)(progRaw)
      case 1 => BFOpt(config)(progRaw)
      case 2 => BFComp(config)(progRaw)
      case n => Failure(EsoExcep(s"bfOpt Out Of Range: $n"))}}
}
