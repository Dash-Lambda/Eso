package lazyk

import common.Config
import lazyk.LazyKFuncs._

import scala.util.Try

object LazyKAnyToIota extends LazyKDialectTranslator{
  val name: String = "LazyK_Iota"
  
  def unapply(config: Config)(prog: String): Try[String] = {
    def mkStr(exp: Expr): String = exp match{
      case AppExpr(e1, e2) => s"*${mkStr(e1)}${mkStr(e2)}"
      case `scomb` => "*i*i*i*ii"
      case `kcomb` => "*i*i*ii"
      case `icomb` => "*ii"
      case `iotaexp` => "i"}
    LazyKParsers.parse(prog).flatMap(expr => Try{mkStr(expr)})}
}
