package languages.lazyk
import common.Config
import languages.lazyk.LazyKFuncs._

import scala.util.Try

object LazyKAnyToJot extends LazyKDialectTranslator{
  val name: String = "LazyK_Jot"
  
  def unapply(config: Config)(prog: String): Try[String] = {
    def mkStr(exp: Expr): String = exp match{
      case AppExpr(e1, e2) => s"1${mkStr(e1)}${mkStr(e2)}"
      case FuncExpr(fun) => fun match{
        case `scomb` => "11111000"
        case `kcomb` => "11100"
        case `icomb` => "11111110001110011100"
        case `iotacomb` => "11111110001111111000111111100011100111001111001111100011110011100"}}
    LazyKParsers.parse(prog).flatMap(expr => Try{mkStr(expr)})}
}
