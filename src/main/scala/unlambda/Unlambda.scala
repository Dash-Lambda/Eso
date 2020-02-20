package unlambda

import common.{Config, Interpreter}
import parsers.{DepthRecurParser, EsoParser, PartialParser}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.control.TailCalls._
import scala.util.Try

object Unlambda extends Interpreter{
  val name: String = "Unlambda"
  
  val funcParser: EsoParser[Vector[Char], Expr] = {
    val funcMap = immutable.HashMap(
      'i' -> I,
      'v' -> V,
      'k' -> K,
      's' -> S,
      'c' -> C,
      'd' -> D,
      'r' -> OUT('\n'),
      '@' -> AT,
      '|' -> PIPE,
      'e' -> E)
    PartialParser.simple{
      case '.' +: c +: cs => (FuncExpr(OUT(c)), cs)
      case '?' +: c +: cs => (FuncExpr(QUES(c)), cs)
      case f +: cs if funcMap.isDefinedAt(f) => (FuncExpr(funcMap(f)), cs)}}
  val unlParser: EsoParser[Vector[Char], Expr] = {
    val toks = "ivkscdr@|e`.?#".toVector
    @tailrec
    def condition(src: Vector[Char], ac: Vector[Char] = Vector()): Vector[Char] = src match{
      case '.' +: c +: cs => condition(cs, ac :+ '.' :+ c)
      case '?' +: c +: cs => condition(cs, ac :+ '?' :+ c)
      case '#' +: cs => condition(cs.dropWhile(_ != '\n'), ac)
      case c +: cs if toks.contains(c) => condition(cs, ac :+ c)
      case _ +: cs => condition(cs, ac)
      case _ => ac}
    def recur(src: Vector[Char]): Option[(Vector[Char], Int, Int)] = src match{
      case '`' +: cs => Some((cs, 0, 1))
      case _ => None}
    def collect(xs: Seq[Expr]): Expr = xs match{
      case x +: y +: _ => AppExpr(x, y)}
    DepthRecurParser(funcParser)(2)(recur)(collect).withConditioning(src => condition(src))}
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = unlParser(progRaw.toVector).toTry("Invalid Unlambda Expression") map {prog =>
    inputs => LazyList.unfold(prog(EndCont, Env(None, inputs)))(nxt => nxt.result.resolve)}
  
  case class Env(cur: Option[Char], inp: Seq[Char]){
    def read: Env = inp match{
      case c +: cs => Env(Some(c), cs)
      case _ => Env(None, inp)}}
  
  trait Ret{
    def resolve: Option[(Char, TailRec[Ret])]}
  
  trait Expr{
    def apply(cc: Cont, env: Env): TailRec[Ret]}
  
  trait Cont{
    def apply(f: Func, env: Env): TailRec[Ret]}
  
  trait Func{
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret]}
  
  //Returns
  object HaltRet extends Ret{
    def resolve: Option[(Char, TailRec[Ret])] = None}
  
  case class PrintRet(c: Char, nxt: TailRec[Ret]) extends Ret{
    def resolve: Option[(Char, TailRec[Ret])] = Some((c, nxt))}
  
  //Expressions
  case class FuncExpr(f: Func) extends Expr{
    def apply(cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(f, env))}
  
  case class AppExpr(x: Expr, y: Expr) extends Expr{
    def apply(cc: Cont, env: Env): TailRec[Ret] = tailcall(x(ExprCont(y, cc), env))}
  
  //Continuations
  object EndCont extends Cont{
    def apply(f: Func, env: Env): TailRec[Ret] = done(HaltRet)}
  
  case class ExprCont(y: Expr, cc: Cont) extends Cont{
    def apply(f: Func, env: Env): TailRec[Ret] = f match{
      case D => tailcall(cc(D1(y), env))
      case _ => tailcall(y(FuncCont(f, cc), env))}}
  
  case class FuncCont(x: Func, cc: Cont) extends Cont{
    def apply(f: Func, env: Env): TailRec[Ret] = tailcall(x(f, cc, env))}
  
  case class DCont(y: Func, cc: Cont) extends Cont{
    def apply(f: Func, env: Env): TailRec[Ret] = tailcall(f(y, cc, env))}
  
  //Functions
  object I extends Func{
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(f, env))}
  
  object V extends Func {
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(V, env))}
  
  case class OUT(c: Char) extends Func{
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = done(PrintRet(c, tailcall(cc(f, env))))}
  
  case class D1(x: Expr) extends Func{
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = tailcall(x(DCont(f, cc), env))}
  object D extends Func{
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(D1(FuncExpr(f)), env))}
  
  case class S2(x: Func, y: Func) extends Func{
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = {
      val expr = AppExpr(AppExpr(FuncExpr(x), FuncExpr(f)), AppExpr(FuncExpr(y), FuncExpr(f)))
      tailcall(expr(cc, env))}}
  case class S1(x: Func) extends Func{
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(S2(x, f), env))}
  object S extends Func {
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(S1(f), env))}
  
  case class K1(x: Func) extends Func{
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(x, env))}
  object K extends Func {
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(K1(f), env))}
  
  case class C1(cc1: Cont) extends Func{
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = tailcall(cc1(f, env))}
  object C extends Func {
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = tailcall(f(C1(cc), cc, env))}
  
  object E extends Func {
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = done(HaltRet)}
  
  object AT extends Func {
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = env.cur match{
      case None => tailcall(f(V, cc, env.read))
      case _ => tailcall(f(I, cc, env.read))}}
  
  case class QUES(c: Char) extends Func{
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = env.cur match{
      case Some(`c`) => tailcall(f(I, cc, env))
      case _ => tailcall(f(V, cc, env))}}
  
  object PIPE extends Func {
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = env.cur match{
      case Some(c) => tailcall(f(OUT(c), cc, env))
      case None => tailcall(f(V, cc, env))}}
}