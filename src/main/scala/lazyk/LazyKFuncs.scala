package lazyk

import scala.util.control.TailCalls._

object LazyKFuncs {
  val churchTrue: Func = K
  val churchFalse: Func = K1(FuncExpr(I))
  
  trait Expr{
    def apply(cc: Cont): TailRec[Func]}
  
  trait Cont{
    def apply(f: Func): TailRec[Func]}
  
  trait Func{
    def apply(f: Expr, cc: Cont): TailRec[Func]}
  
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
  object I extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(f(cc))}
  
  case class K1(x: Expr) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(x(cc))}
  object K extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(cc(K1(f)))}
  
  case class S2(x: Expr, y: Expr) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = {
      val fex = new DupExpr(f)
      tailcall(x(ExprCont(fex, ExprCont(AppExpr(y, fex), cc))))}}
  case class S1(x: Expr) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(cc(S2(x, f)))}
  object S extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(cc(S1(f)))}
  
  case class ChurchNum1(num: Int, fexp: Expr) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall{
      Iterator.fill(num)(fexp).foldLeft(f){case (y, x) => AppExpr(x, y)}(cc)}}
  case class ChurchNum(num: Int) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(cc(ChurchNum1(num, f)))}
  
  case class Pair(x: Expr, y: Expr) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(f(ExprCont(x, ExprCont(y, cc))))}
  
  case class ChurchList(lst: Seq[Int]) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = lst match{
      case n +: ns => tailcall(Pair(FuncExpr(ChurchNum(n)), FuncExpr(ChurchList(ns)))(f, cc))}}
  
  object ChurchHalt extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = done(ChurchCounter)}
  case class ChurchReturn(exp: Expr) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = done(this)}
  object ChurchCounter extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = done(ChurchReturn(f))}
}
