package lazybird

import common.{Config, Interpreter}
import parsers.{EsoParser, PartialArbitraryScanParser}

import scala.util.Try
import scala.util.control.TailCalls._

object LazyBird extends Interpreter{
  val name: String = "LazyBird"
  
  val lzbParser: EsoParser[Seq[Char], Expr] = {
    val s: Expr = FE(S)
    val k: Expr = FE(K)
    val i: Expr = FE(I)
    PartialArbitraryScanParser[Char, Expr](_.headOption){
      case (cs :+ '.' :+ c, ac) => (cs, FE(PrintComb(c)) +: ac)
      case (cs :+ '^' :+ c, a +: ac) => (cs, a.elim(c.toLower) +: ac)
      case (cs :+ '`', x +: y +: ac) => (cs, AppExpr(x, y) +: ac)
      case (cs :+ c, ac) =>
        val expr = c.toLower match{
          case 's' => s
          case 'k' => k
          case 'i' => i
          case 'm' => AE(AE(s, i), i)
          case '0' => AE(k, i)
          case 'w' => AE(AE(s, s), AE(k, i))
          case 'u' => AE(AE(s, AE(k, AE(s, i))), AE(AE(s, i), i))
          case 'o' => AE(s, i)
          case 't' => AE(AE(s, AE(k, AE(s, i))), k)
          case 'l' => AE(AE(s, AE(AE(s, AE(k, s)), k)), AE(k, AE(AE(s, i), i)))
          case 'b' => AE(AE(s, AE(k, s)), k)
          case 'c' => AE(AE(s, AE(AE(s, AE(k, AE(AE(s, AE(k, s)), k))), s)), AE(k, k))
          case 'q' => AE(AE(s, AE(k, AE(s, AE(AE(s, AE(k, s)), k)))), k)
          case 'v' => AE(AE(s, AE(k, AE(AE(s, AE(AE(s, AE(k, AE(AE(s, AE(k, s)), k))), s)), AE(k, k)))), AE(AE(s, AE(k, AE(s, i))), k))
          case '@' => AE(AE(s, AE(AE(s, i), AE(k, s))), AE(k, k))
          case '#' => FE(V)
          case '_' => FE(Read)
          case '&' => FE(Eql)
          case 'r' => FE(PrintComb('\n'))
          case cl => CharExpr(cl)}
        (cs, expr +: ac)}.withErrors}
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = lzbParser(progRaw.toVector).toTry() map{ prog =>
    inputs => LazyList.unfold(tailcall(prog(endCont, Env(inputs))))(nxt => nxt.result.resolve)}
  
  case class Env(inp: Seq[Char]){
    def read: (Func, Env) = (ChurchNum(inp.head.toInt), Env(inp.tail))}
  
  trait Ret{
    def resolve: Option[(Char, TailRec[Ret])]}
  trait Expr{
    def apply(cc: Cont, env: Env): TailRec[Ret]
    def elim(c: Char): Expr}
  abstract class Cont{
    def apply(f: Func, env: Env): TailRec[Ret]}
  abstract class Func {
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret]}
  
  //Returns
  object HaltRet extends Ret{
    def resolve: Option[(Char, TailRec[Ret])] = None}
  
  case class CharRet(c: Char, nxt: TailRec[Ret]) extends Ret{
    def resolve: Option[(Char, TailRec[Ret])] = Some((c, nxt))}
  
  //Expressions
  def AE(x: Expr, y: Expr): AppExpr = AppExpr(x, y)
  case class AppExpr(x: Expr, y: Expr) extends Expr{
    def apply(cc: Cont, env: Env): TailRec[Ret] = tailcall(x(exprCont(y, cc), env))
    def elim(c: Char): Expr = AE(AE(FE(S), x.elim(c)), y.elim(c))}
  
  def FE(f: Func): FuncExpr = FuncExpr(f)
  case class FuncExpr(f: Func) extends Expr{
    def apply(cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(f, env))
    def elim(c: Char): Expr = AE(FE(K), FE(f))}
  
  case class CharExpr(c1: Char) extends Expr{
    def apply(cc: Cont, env: Env): TailRec[Ret] = done(HaltRet)
    def elim(c: Char): Expr = {
      if(c == c1) FE(I)
      else AE(FE(K), this)}}
  
  //Continuations
  val endCont: Cont = (_, _) => done(HaltRet)
  def exprCont(y: Expr, cc: Cont): Cont = (f, env) => tailcall(f(y, cc, env))
  
  //Functions
  object I extends Func {
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = tailcall(f(cc, env))}
  
  object V extends Func {
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(V, env))}
  
  case class S2(x: Expr, y: Expr) extends Func{
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = tailcall(x(exprCont(f, exprCont(AE(y, f), cc)), env))}
  case class S1(x: Expr) extends Func{
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(S2(x, f), env))}
  object S extends Func {
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(S1(f), env))}
  
  case class K1(x: Expr) extends Func{
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = tailcall(x(cc, env))}
  object K extends Func {
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(K1(f), env))}
  
  case class ChurchNum1(num: Int, x: Expr) extends Func{
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = tailcall(Iterator.fill(num)(x).foldLeft(f){case (b, a) => AE(a, b)}(cc, env))}
  case class ChurchNum(num: Int) extends Func{
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(ChurchNum1(num, f), env))}
  
  case class PrintComb(c: Char) extends Func{
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = done(CharRet(c, tailcall(f(cc, env))))}
  
  object Read extends Func {
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = env.read match{
      case (cn, nenv) => tailcall(AE(FE(cn), f)(cc, nenv))}}
  
  case class E1(x: Expr) extends Func{
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = {
      if(f == x) tailcall(cc(K, env))
      else tailcall(AE(FE(K), FE(I))(cc, env))}}
  object Eql extends Func {
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(E1(f), env))}
}
