package interpreters

import translators.BFTranslator

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object BFManager {
  def apply(translators: mutable.HashMap[String, BFTranslator], lang: String, flags: Vector[Boolean], nums: Vector[Int])(progRaw: String): Try[String] = Try{nums(2)} match{
    case Success(bfOpt) =>
      val baseInterp: String => Try[String] = bfOpt match{
        case 0 => BFFunctional(flags, nums)
        case 1 => BFOptimized(flags, nums)
        case 2 => BFCompiled(flags, nums)
      }
      if(lang == "BrainFuck") baseInterp(progRaw)
      else translators.get(lang) match{
        case Some(trans) => baseInterp(trans(progRaw))
        case None => Failure(InterpreterException(s"Not a Recognized Translator: $lang"))
      }
    case Failure(_) => Failure(InterpreterException("Missing Config Values"))
  }
}