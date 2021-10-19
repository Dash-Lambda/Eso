package languages.lazyk

import common.Config
import languages.lazyk.LazyKFuncs._

import scala.util.Try

object LazyKAnyToCC extends LazyKDialectTranslator{
  val name: String = "LazyK_CC"
  
  def unapply(config: Config)(prog: String): Try[String] = {
    def mkStr(exp: Expr): String = exp match{
      case AppExpr(e1, e2) => e2 match{
        case FuncExpr(_) => s"${mkStr(e1)}${mkStr(e2)}"
        case _ => s"${mkStr(e1)}(${mkStr(e2)})"}
      case FuncExpr(fun) => fun match{
        case `scomb` => "S"
        case `kcomb` => "K"
        case `icomb` => "I"
        case `iotacomb` => "(S((S(I))(KS)))(KK)"}}
    LazyKParsers.parse(prog).flatMap(expr => Try{mkStr(expr)})}
}
