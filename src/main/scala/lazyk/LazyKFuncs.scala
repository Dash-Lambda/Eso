package lazyk

import scala.util.control.TailCalls._

object LazyKFuncs {
  val icomb: Func = (f, cc) => tailcall(f(cc))
  val kcomb: Func = (x, c1) => tailcall(c1((_, c2) => x(c2)))
  val scomb: Func = (x, c1) => tailcall(
    c1((y, c2) =>
      tailcall(c2{(z, c3) =>
        val zex = new DupExpr(z)
        tailcall(x(ExprCont(zex, ExprCont(AppExpr(y, zex), c3))))})))
  
  val sexp: Expr = FuncExpr(scomb)
  val kexp: Expr = FuncExpr(kcomb)
  val iexp: Expr = FuncExpr(icomb)
  
  val churchPair: (Expr, Expr) => Func = (x, y) => (f, cc) => tailcall(f(ExprCont(x, ExprCont(y, cc))))
  val churchList: Seq[Int] => Func = lst => (f, cc) => tailcall(churchPair(FuncExpr(churchNum(lst.head)), FuncExpr(churchList(lst.tail)))(f, cc))
  val churchNum: Int => Func = num => (f, c1) => tailcall(c1((x, c2) => Iterator.fill(num)(f).foldLeft(x){case (b, a) => AppExpr(a, b)}(c2)))
  val churchTrue: Func = (x, c1) => tailcall(c1((_, c2) => x(c2)))
  val churchFalse: Func = (_, c1) => tailcall(c1((y, c2) => y(c2)))
  
  abstract class Expr extends (Cont => TailRec[Func])
  abstract class Cont extends (Func => TailRec[Func])
  abstract class Func extends ((Expr, Cont) => TailRec[Func])
  
  //Expressions
  case class FuncExpr(f: Func) extends Expr{
    def apply(cc: Cont): TailRec[Func] = tailcall(cc(f))}
  
  case class AppExpr(x: Expr, y: Expr) extends Expr{
    def apply(cc: Cont): TailRec[Func] = tailcall(x(ExprCont(y, cc)))}
  
  class DupExpr(var f: Expr) extends Expr{
    def apply(cc: Cont): TailRec[Func] = f match{
      case FuncExpr(fun) => tailcall(cc(fun))
      case _ => tailcall(f(DupCont(this, cc)))}
    def collapse(res: Func): Unit = f match{
      case FuncExpr(_) => ()
      case _ => f = FuncExpr(res)}}
  
  //Continuations
  object EndCont extends Cont{
    def apply(f: Func): TailRec[Func] = done(f)}
  
  case class ExprCont(y: Expr, cc: Cont) extends Cont{
    def apply(f: Func): TailRec[Func] = tailcall(f(y, cc))}
  
  case class DupCont(u: DupExpr, cc: Cont) extends Cont{
    def apply(f: Func): TailRec[Func] = {
      u.collapse(f)
      tailcall(cc(f))}}
  
  //Funcs
  object ChurchHalt extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = done(ChurchCounter)}
  case class ChurchReturn(exp: Expr) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = done(this)}
  object ChurchCounter extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = done(ChurchReturn(f))}
}
