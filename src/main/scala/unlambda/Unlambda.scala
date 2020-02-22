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
      'i' -> icomb,
      'v' -> vcomb,
      'k' -> kcomb,
      's' -> scomb,
      'c' -> ccomb,
      'd' -> D,
      'r' -> outcomb('\n'),
      '@' -> atcomb,
      '|' -> pipecomb,
      'e' -> ecomb)
    PartialParser.simple{
      case '.' +: c +: cs => (FuncExpr(outcomb(c)), cs)
      case '?' +: c +: cs => (FuncExpr(quescomb(c)), cs)
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
  
  abstract class Ret{
    def resolve: Option[(Char, TailRec[Ret])]}
  abstract class Expr extends ((Cont, Env) => TailRec[Ret])
  abstract class Cont extends ((Func, Env) => TailRec[Ret])
  abstract class Func extends ((Func, Cont, Env) => TailRec[Ret])
  
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
  def icomb: Func = (f, cc, env) => tailcall(cc(f, env))
  def scomb: Func = (x, c1, en1) => tailcall(c1((y, c2, en2) => tailcall(c2((z, c3, en3) => tailcall(AppExpr(AppExpr(FuncExpr(x), FuncExpr(z)), AppExpr(FuncExpr(y), FuncExpr(z)))(c3, en3)), en2)), en1))
  def kcomb: Func = (x, c1, en1) => tailcall(c1((_, c2, en2) => tailcall(c2(x, en2)), en1))
  def vcomb: Func = (_, cc, env) => tailcall(cc(vcomb, env))
  def outcomb(c: Char): Func = (f, cc, env) => done(PrintRet(c, tailcall(cc(f, env))))
  def D1(x: Expr): Func = (f, cc, env) => tailcall(x(DCont(f, cc), env))
  def ccomb: Func = (f, c1, en1) => tailcall(f((g, _, en2) => c1(g, en2), c1, en1))
  def ecomb: Func = (_, _, _) => done(HaltRet)
  def atcomb: Func = {
    (f, cc, env) => env.cur match{
      case None => tailcall(f(vcomb, cc, env.read))
      case _ => tailcall(f(icomb, cc, env.read))}}
  def quescomb(c: Char): Func = {
    (f, cc, env) => env.cur match{
      case Some(`c`) => tailcall(f(icomb, cc, env))
      case _ => tailcall(f(vcomb, cc, env))}}
  def pipecomb: Func = {
    (f, cc, env) => env.cur match{
      case Some(c) => tailcall(f(outcomb(c), cc, env))
      case None => tailcall(f(vcomb, cc, env))}}
  
  object D extends Func{
    def apply(f: Func, cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(D1(FuncExpr(f)), env))}
}