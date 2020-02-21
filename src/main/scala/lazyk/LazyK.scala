package lazyk

import common.{Config, EsoExcep, Interpreter}
import lazyk.LazyKFuncs._

import scala.annotation.tailrec
import scala.util.{Failure, Try}

object LazyK extends Interpreter{
  val name: String = "LazyK"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = LazyKParsers.parse(progRaw) flatMap{initExpr =>
    Try{
      inputs =>
        val cinp = churchList(inputs.to(LazyList).map(_.toInt).takeWhile(_ <= 255) #::: LazyList.continually(256))
        val fexpr: Expr = AppExpr(initExpr, FuncExpr(cinp))
        LazyList.unfold(fexpr)(run)}}
  
  def run(expr: Expr): Option[(Char, Expr)] = {
    val cur = FuncExpr(eval(expr))
    val cexpr = {
      AppExpr(
        AppExpr(
          AppExpr(cur, FuncExpr(churchTrue)),
          FuncExpr(ChurchCounter)),
        FuncExpr(ChurchHalt))}
    val num = countChurchNum(eval(cexpr))
    if(0 > num || num > 255) None
    else{
      val nexpr = AppExpr(cur, FuncExpr(churchFalse))
      Some((num.toChar, nexpr))}}
  
  @tailrec
  def countChurchNum(fun: Func, ac: Int = 0): Int = {
    fun match{
      case ChurchReturn(nexp) => countChurchNum(eval(nexp), ac + 1)
      case ChurchHalt => ac
      case _ => -1}}
  
  def eval(expr: Expr): Func = expr(EndCont).result
}