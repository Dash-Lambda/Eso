package lazyk

import common.AbstractionEliminator
import parsers.EsoParser
import parsers.EsoParser._

object LambdaToLazyKUnl extends AbstractionEliminator{
  val dst: String = "LazyK_Unlambda"
  
  val parser: EsoParser[Expr] = {
    def junkParse: EsoParser[String] = R("^[ \t\r\n]*".r)
    def abstraction: EsoParser[Expr] = S("^") &> (R("^.".r) <&> expression) map {case (c, a) => a.elim(c.head)}
    def application: EsoParser[Expr] = S("`") &> (expression <&> expression) map {case (x, y) => AppExpr(x, y)}
    def character: EsoParser[Expr] = R("^.".r) map (s => CharExpr(s.head))
    def expression: EsoParser[Expr] = junkParse &> (application | abstraction | character)
    expression}
  
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
