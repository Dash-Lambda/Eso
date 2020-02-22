package lazyk

import common.Config
import lazyk.LazyKFuncs.mkUnlambdaString

import scala.util.Try

object LazyKAnyToUnl extends LazyKDialectTranslator{
  val name: String = "LazyK_Unlambda"
  
  def unapply(config: Config)(prog: String): Try[String] = LazyKParsers.parse(prog).flatMap(expr => Try{mkUnlambdaString(expr)})
}
