package lazyk

import common.{Config, Interpreter}
import lazyk.LazyKFuncs._

import scala.annotation.tailrec
import scala.util.Try

object LazyK extends Interpreter{
  val name: String = "LazyK"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = LazyKParsers.parse(progRaw) flatMap{initExpr =>
    Try{inputs =>
      val cinp = churchList(inputs.to(LazyList).map(_.toInt).takeWhile(_ <= 255) #::: LazyList.continually(256))
      val fexpr: Expr = AppExpr(initExpr, FuncExpr(cinp))
      LazyList.unfold(fexpr)(run)}}
  
  def run(expr: Expr): Option[(Char, Expr)] = {
    val cur = FuncExpr(eval(expr))
    val num = countChurchNum(cur)
    if(0 > num || num > 255) None
    else{
      val nexpr = AppExpr(cur, FuncExpr(churchFalse))
      Some((num.toChar, nexpr))}}
  
  def countChurchNum(fun: Expr): Int = {
    @tailrec
    def cdo(f: Func, ac: Int = 0): Int = f match{
      case ChurchLevel(nxt) => cdo(nxt(), ac + 1)
      case `churchHalt` => ac
      case _ => -1}
    cdo(eval(
      AppExpr(
        AppExpr(
          AppExpr(fun, FuncExpr(churchTrue)),
          FuncExpr(churchCounter)),
        FuncExpr(churchHalt))))}
  
  def eval(expr: Expr): Func = expr(endCont).result
}