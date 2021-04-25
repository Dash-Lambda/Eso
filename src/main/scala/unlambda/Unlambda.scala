package unlambda

import common.{Config, Interpreter}
import parsers.EsoParser._
import parsers.EsoParser

import scala.util.Try
import scala.util.control.TailCalls._

object Unlambda extends Interpreter{
  val name: String = "Unlambda"
  
  lazy val exprParse: EsoParser[Expr] = junkParse &> (((printParse | quesParse | combParse) map funcExpr) | appParse)
  val appParse: EsoParser[Expr] = S("`") &> (exprParse <&> exprParse) map {case (x, y) => appExpr(x, y)}
  val printParse: EsoParser[Func] = R("""(?s)^\.(.)""".r) map (c => outcomb(c.head))
  val quesParse: EsoParser[Func] = R("""(?s)^\?(.)""".r) map (c => quescomb(c.head))
  val combParse: EsoParser[Func] = R("""^[ivkscdr@|e]""".r) map {
    case "i" => icomb
    case "v" => vcomb
    case "k" => kcomb
    case "s" => scomb
    case "c" => ccomb
    case "d" => dcomb
    case "r" => outcomb('\n')
    case "@" => atcomb
    case "|" => pipecomb
    case "e" => ecomb}
  val junkParse: EsoParser[String] = R("""^[^ivkscdr@|e`.?#]*(?:#.*\v)?[^ivkscdr@|e`.?#]*""".r)
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    exprParse(progRaw).toTry("Invalid Unlambda Expression") map {case (prog, _, _, _) =>
      inputs => LazyList.unfold(prog(endCont, Env(None, inputs)))(_.result.resolve)}}
  
  case class Env(cur: Option[Char], inp: Seq[Char]){
    def read: Env = inp match{
      case c +: cs => Env(Some(c), cs)
      case _ => Env(None, inp)}}
  
  abstract class Ret{
    def resolve: Option[(Char, TailRec[Ret])]}
  abstract class Cont extends ((Func, Env) => TailRec[Ret])
  abstract class Expr extends ((Cont, Env) => TailRec[Ret])
  abstract class Func extends ((Func, Cont, Env) => TailRec[Ret])
  
  //Returns
  object HaltRet extends Ret{
    def resolve: Option[(Char, TailRec[Ret])] = None}
  
  case class PrintRet(c: Char, nxt: TailRec[Ret]) extends Ret{
    def resolve: Option[(Char, TailRec[Ret])] = Some((c, nxt))}
  
  //Continuations
  def endCont: Cont = (_, _) => done(HaltRet)
  def funcCont(x: Func, cc: Cont): Cont = (y, env) => tailcall(x(y, cc, env))
  def dCont(y: Func, cc: Cont): Cont = (x, env) => tailcall(x(y, cc, env))
  def exprCont(y: Expr, cc: Cont): Cont = {
    (f, env) => tailcall(
      if(f == dcomb) cc(D1(y), env)
      else y(funcCont(f, cc), env))}
  
  //Expressions
  def funcExpr(f: Func): Expr = (cc, env) => tailcall(cc(f, env))
  def appExpr(x: Expr, y: Expr): Expr = (cc, env) => tailcall(x(exprCont(y, cc), env))
  def funcAppExpr(x: Func, y: Func): Expr = (cc, env) => tailcall(x(y, cc, env))
  
  //Functions
  def scomb: Func = {
    (x, c1, e1) => tailcall(
      c1((y, c2, e2) => tailcall(
        c2((z, c3, e3) => tailcall(
          x(z, exprCont(funcAppExpr(y, z), c3), e3)), e2)), e1))}
  def kcomb: Func = {
    (x, c1, en1) => tailcall(
      c1((_, c2, en2) => tailcall(
        c2(x, en2)), en1))}
  def icomb: Func = (f, cc, env) => tailcall(cc(f, env))
  def vcomb: Func = (_, cc, env) => tailcall(cc(vcomb, env))
  def outcomb(c: Char): Func = (f, cc, env) => done(PrintRet(c, tailcall(cc(f, env))))
  def ccomb: Func = (f, c1, en1) => tailcall(f((g, _, en2) => c1(g, en2), c1, en1))
  def ecomb: Func = (_, _, _) => done(HaltRet)
  def atcomb: Func = (f, cc, env) => tailcall(f(if(env.cur.isEmpty) vcomb else icomb, cc, env.read))
  def quescomb(c: Char): Func = (f, cc, env) => tailcall(f(if(env.cur.contains(c)) icomb else vcomb, cc, env))
  def pipecomb: Func = (f, cc, env) => tailcall(f(env.cur map outcomb getOrElse vcomb, cc, env))
  
  def D1(x: Expr): Func = (f, cc, env) => tailcall(x(dCont(f, cc), env))
  lazy val dcomb: Func = (f, cc, env) => tailcall(cc(D1(funcExpr(f)), env))
}