package lazyk

import common.{Config, Translator}
import lazyk.LazyKFuncs.{AppExpr, Expr, FuncExpr, I, K, S}

import scala.util.Try

object LazyKUnlToCC extends Translator{
  val name: String = "LazyK_Unlambda"
  val baseLang: String = "LazyK_CC"
  
  def apply(config: Config)(prog: String): Try[String] = {
    val expr = LazyKParsers.unlParser.parseOne(prog)
    def mkstr(exp: Expr): String = exp match{
      case AppExpr(e1, e2) => s"(${mkstr(e1)}${mkstr(e2)})"
      case FuncExpr(f) => f match{
        case S => "S"
        case K => "K"
        case I => "I"}}
    Try{mkstr(expr)}}
  
  def unapply(config: Config)(prog: String): Try[String] = {
    val expr = LazyKParsers.combParser.parseOne(prog)
    def mkstr(exp: Expr): String = exp match{
      case AppExpr(e1, e2) => s"`${mkstr(e1)}${mkstr(e2)}"
      case FuncExpr(f) => f match{
        case S => "s"
        case K => "k"
        case I => "i"}}
    Try{mkstr(expr)}}
}
