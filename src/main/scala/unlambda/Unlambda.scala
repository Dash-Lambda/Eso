package unlambda

import common.{Config, Interpreter}
import parsers.{EsoParser, PartialArbitraryScanParser}

import scala.annotation.tailrec
import scala.util.Try
import scala.util.control.TailCalls._

object Unlambda extends Interpreter{
  val name: String = "Unlambda"
  
  val unlParser: EsoParser[Seq[Char], Expr] = {
    @tailrec
    def condition(src: Seq[Char], ac: Vector[Char] = Vector()): Vector[Char] = src match{
      case '.' +: c +: cs => condition(cs, ac :+ '.' :+ (-c).toChar)
      case '?' +: c +: cs => condition(cs, ac :+ '?' :+ (-c).toChar)
      case '#' +: cs => condition(cs.dropWhile(_ != '\n'), ac)
      case c +: cs if "ivkscdr@|e`.?#".contains(c) => condition(cs, ac :+ c)
      case _ +: cs => condition(cs, ac)
      case _ => ac}
    PartialArbitraryScanParser[Char, Expr](_.headOption){
      case (cs :+ '.' :+ c, ac) => (cs, FuncExpr(outcomb((-c).toChar)) +: ac)
      case (cs :+ '?' :+ c, ac) => (cs, FuncExpr(quescomb((-c).toChar)) +: ac)
      case (cs :+ '`', x +: y +: ac) => (cs, AppExpr(x, y) +: ac)
      case (cs :+ c, ac) =>
        val fun = c match{
          case 'i' => icomb
          case 'v' => vcomb
          case 'k' => kcomb
          case 's' => scomb
          case 'c' => ccomb
          case 'd' => D
          case 'r' => outcomb('\n')
          case '@' => atcomb
          case '|' => pipecomb
          case 'e' => ecomb}
        (cs, FuncExpr(fun) +: ac)}
      .withConditioning(src => condition(src))}
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = unlParser(progRaw.toVector).toTry("Invalid Unlambda Expression") map {prog =>
    inputs => LazyList.unfold(prog(endCont, Env(None, inputs)))(nxt => nxt.result.resolve)}
  
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
    def apply(cc: Cont, env: Env): TailRec[Ret] = tailcall(x(exprCont(y, cc), env))}
  
  //Continuations
  def endCont: Cont = (_, _) => done(HaltRet)
  def funcCont(x: Func, cc: Cont): Cont = (y, env) => tailcall(x(y, cc, env))
  def dCont(y: Func, cc: Cont): Cont = (x, env) => tailcall(x(y, cc, env))
  def exprCont(y: Expr, cc: Cont): Cont = {
    (f, env) => f match{
      case D => tailcall(cc(D1(y), env))
      case _ => tailcall(y(funcCont(f, cc), env))}}
  
  //Functions
  def icomb: Func = (f, cc, env) => tailcall(cc(f, env))
  def scomb: Func = (x, c1, en1) => tailcall(c1((y, c2, en2) => tailcall(c2((z, c3, en3) => tailcall(AppExpr(AppExpr(FuncExpr(x), FuncExpr(z)), AppExpr(FuncExpr(y), FuncExpr(z)))(c3, en3)), en2)), en1))
  def kcomb: Func = (x, c1, en1) => tailcall(c1((_, c2, en2) => tailcall(c2(x, en2)), en1))
  def vcomb: Func = (_, cc, env) => tailcall(cc(vcomb, env))
  def outcomb(c: Char): Func = (f, cc, env) => done(PrintRet(c, tailcall(cc(f, env))))
  def D1(x: Expr): Func = (f, cc, env) => tailcall(x(dCont(f, cc), env))
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