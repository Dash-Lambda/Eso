package interpreters

import translators.BFTranslator

import scala.collection.mutable
import scala.util.{Failure, Try}

object BFManager {
  def apply(translators: mutable.HashMap[String, BFTranslator], initTapeSize: Int, outputMaxLength: Int, optimized: Boolean, log: Boolean, lang: String)(prog: String): Try[String] = {
    val baseInterp: String => Try[String] = if(optimized) BFOptimized(initTapeSize, outputMaxLength, log) else BFFunctional(initTapeSize, outputMaxLength, log)
    lang match{
      case "BrainFuck" => baseInterp(prog)
      case _ if translators.isDefinedAt(lang) => baseInterp(translators(lang)(prog))
      case _ => Failure(InterpreterException("Language Not Recognized"))
    }
  }
}
