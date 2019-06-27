package interpreters

import translators.BFTranslator

import scala.collection.mutable
import scala.util.{Failure, Try}

object BFManager {
  def apply(translators: mutable.HashMap[String, BFTranslator], initTapeSize: Int, outputMaxLength: Int, optimized: Boolean, dynamicTapeSize: Boolean, log: Boolean, debug: Boolean, lang: String)(prog: String): Try[String] = {
    val baseInterp: String => Try[String] = {
      if(optimized) BFOptimized(initTapeSize, outputMaxLength, dynamicTapeSize, log, debug)
      else BFFunctional(initTapeSize, outputMaxLength, dynamicTapeSize, log, debug)
    }
    
    lang match{
      case "BrainFuck" => baseInterp(prog)
      case _ if translators.isDefinedAt(lang) => baseInterp(translators(lang)(prog))
      case _ => Failure(InterpreterException("Language Not Recognized"))
    }
  }
}