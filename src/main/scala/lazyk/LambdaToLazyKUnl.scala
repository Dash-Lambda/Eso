package lazyk

import common.AbstractionEliminator
import parsers.{EsoParser, PartialArbitraryScanParser}

object LambdaToLazyKUnl extends AbstractionEliminator{
  val dst: String = "LazyK_Unlambda"
  
  val parser: EsoParser[Seq[Char], Expr] = {
    PartialArbitraryScanParser[Char, Expr](_.headOption){
      case (cs :+ '^' :+ c, a +: ac) => (cs, a.elim(c) +: ac)
      case (cs :+ '`', x +: y +: ac) => (cs, AppExpr(x, y) +: ac)
      case (cs :+ c, ac) =>
        if(" \t\r\n".contains(c)) (cs, ac)
        else (cs, CharExpr(c) +: ac)}
      .withErrors}
  
  case class AppExpr(x: Expr, y: Expr) extends Expr {
    def elim(c: Char): Expr =
      if(uses(c)) AppExpr(AppExpr(CharExpr('s'), x.elim(c)), y.elim(c))
      else AppExpr(CharExpr('k'), this)
    def uses(c: Char): Boolean = x.uses(c) || y.uses(c)
    override def toString: String = s"`$x$y"}
  
  case class CharExpr(chk: Char) extends Expr {
    def elim(c: Char): Expr =
      if(c == chk) CharExpr('i')
      else AppExpr(CharExpr('k'), this)
    def uses(c: Char): Boolean = c == chk
    override def toString: String = chk.toString}
}
