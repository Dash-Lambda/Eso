package languages.lazyk

import common.{Config, Translator}
import languages.lazyk.LazyKFuncs.mkUnlambdaString

import scala.util.Try

abstract class LazyKDialectTranslator extends Translator{
  val baseLang: String = "LazyK"
  
  def apply(config: Config)(prog: String): Try[String] = LazyKParsers.parse(prog).flatMap(expr => Try{mkUnlambdaString(expr)})
}
