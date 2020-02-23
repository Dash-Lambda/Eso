package lazyk

import scala.util.control.TailCalls._

object LazyKFuncs {
  def mkUnlambdaString(exp: Expr): String = exp match{
    case AppExpr(e1, e2) => s"`${mkUnlambdaString(e1)}${mkUnlambdaString(e2)}"
    case `scomb` => "s"
    case `kcomb` => "k"
    case `icomb` => "i"
    case `iotaexp` => "``s``si`ks`kk"}
  
  trait Expr extends (Cont => TailRec[Func])
  abstract class Cont extends (Func => TailRec[Func])
  abstract class Func extends ((Expr, Cont) => TailRec[Func]) with Expr{
    def apply(cc: Cont): TailRec[Func] = tailcall(cc(this))}
  
  //Expressions
  case class AppExpr(x: Expr, y: Expr) extends Expr{
    def apply(cc: Cont): TailRec[Func] = tailcall(x(exprCont(y, cc)))}
  
  class DupExpr(var f: Expr) extends Expr{
    def apply(cc: Cont): TailRec[Func] = f match{
      case fun: Func => tailcall(cc(fun))
      case _ => tailcall(f(dupCont(this, cc)))}
    def collapse(res: Func): Unit = f match{
      case _: Func => ()
      case _ => f = res}}
  
  //Continuations
  val endCont: Cont = f => done(f)
  def exprCont(y: Expr, cc: Cont): Cont = x => tailcall(x(y, cc))
  def dupCont(u: DupExpr, cc: Cont): Cont = {
    f =>
      u.collapse(f)
      tailcall(cc(f))}
  
  //Funcs
  val icomb: Func = (f, cc) => tailcall(f(cc))
  val kcomb: Func = (x, c1) => tailcall(c1((_, c2) => tailcall(x(c2))))
  val scomb: Func = (x, c1) => tailcall(c1((y, c2) =>
    tailcall(c2{(z, c3) =>
      val zex = new DupExpr(z)
      tailcall(x(exprCont(zex, exprCont(AppExpr(y, zex), c3))))})))
  
  val churchPair: (Expr, Expr) => Func = (x, y) => (f, cc) => tailcall(f(exprCont(x, exprCont(y, cc))))
  val churchTrue: Func = kcomb
  val churchFalse: Func = (_, c1) => tailcall(c1((y, c2) => y(c2)))
  def churchList(lst: Seq[Int]): Func = (f, cc) => tailcall(churchPair(churchNum(lst.head), churchList(lst.tail))(f, cc))
  def churchNum(num: Int): Func = (f, c1) => tailcall(c1((x, c2) => tailcall(Iterator.fill(num)(f).foldLeft(x){case (b, a) => AppExpr(a, b)}(c2))))
  val iotaexp: Func = churchPair(scomb, kcomb)
  
  val churchFail: Func = (_, _) => done(churchFail)
  val churchHalt: Func = (_, _) => done(churchFail)
  val churchCounter: Func = (f, _) => done(ChurchLevel(() => f(endCont).result))
  case class ChurchLevel(nxt: () => Func) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = done(churchFail)}
}
