package interpreters

import translators.BFTranslator

import scala.collection.mutable
import scala.util.{Failure, Try}

object BFManager {
  def apply(translators: mutable.HashMap[String, BFTranslator],
            initTapeSize: Int, outputMaxLength: Int,
            optimizing: Int, dynamicTapeSize: Boolean, log: Boolean, debug: Boolean,
            lang: String)(prog: String): Try[String] = {
    val baseInterp: String => Try[String] = optimizing match{
      case 0 => BFFunctional(initTapeSize, outputMaxLength, dynamicTapeSize, log, debug)
      case 1 => BFOptimized(initTapeSize, outputMaxLength, dynamicTapeSize, log, debug)
      case 2 => BFCompiled(initTapeSize, outputMaxLength, dynamicTapeSize, log, debug)
    }
    
    lang match{
      case "BrainFuck" => baseInterp(prog)
      case _ if translators.isDefinedAt(lang) => baseInterp(translators(lang)(prog))
      case _ => Failure(InterpreterException("Language Not Recognized"))
    }
  }
}