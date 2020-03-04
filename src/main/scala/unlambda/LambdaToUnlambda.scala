package unlambda

import common.AbstractionEliminator
import parsers.{EsoParser, PartialArbitraryScanParser}

import scala.annotation.tailrec

object LambdaToUnlambda extends AbstractionEliminator{
  val dst: String = "Unlambda"
  val parser: EsoParser[Seq[Char], Expr] = {
    @tailrec
    def cdo(src: Seq[Char], ac: Vector[Char] = Vector()): Vector[Char] = src match{
      case a +: b +: cs if a == '.' || a == '?' => cdo(cs, ac :+ a :+ (-b).toChar)
      case c +: cs => if(" \t\r\n".contains(c)) cdo(cs, ac) else cdo(cs, ac :+ c)
      case _ => ac}
    PartialArbitraryScanParser[Char, Expr](_.headOption){
      case (cs :+ a :+ b, ac) if a == '.' || a == '?' => (cs, IOExpr(a, (-b).toChar) +: ac)
      case (cs :+ '^' :+ c, a +: ac) => (cs, a.elim(c) +: ac)
      case (cs :+ '`', x +: y +: ac) => (cs, AppExpr(x, y) +: ac)
      case (cs :+ c, ac) => (cs, CharExpr(c) +: ac)}
      .withConditioning(inp => cdo(inp))}
  
  case class AppExpr(x: Expr, y: Expr) extends Expr {
    def elim(c: Char): Expr =
      if(uses(c)) AppExpr(AppExpr(CharExpr('s'), x.elim(c)), y.elim(c))
      else AppExpr(CharExpr('k'), AppExpr(CharExpr('d'), this))
    def uses(c: Char): Boolean = x.uses(c) || y.uses(c)
    override def toString: String = s"`$x$y"}
  
  case class CharExpr(chk: Char) extends Expr {
    def elim(c: Char): Expr =
      if(c == chk) CharExpr('i')
      else AppExpr(CharExpr('k'), this)
    def uses(c: Char): Boolean = c == chk
    override def toString: String = chk.toString}
  
  case class IOExpr(op: Char, arg: Char) extends Expr {
    def elim(c: Char): Expr = AppExpr(CharExpr('k'), AppExpr(CharExpr('d'), this))
    def uses(c: Char): Boolean = false
    override def toString: String = s"$op$arg"}
}
