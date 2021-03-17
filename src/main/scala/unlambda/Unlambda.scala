package unlambda

import common.{Config, Interpreter}
import parsers.EsoParser
import parsers.Implicits._

import scala.util.Try
import scala.util.control.TailCalls._

object Unlambda extends Interpreter{
  val name: String = "Unlambda"
  
  def exprParse: EsoParser[Expr] = junkParse &> (((printParse | quesParse | combParse) map funcExpr) | appParse)
  def appParse: EsoParser[Expr] = "`" &> (exprParse <&> exprParse) map {case (x, y) => appExpr(x, y)}
  def printParse: EsoParser[Func] = "." &> """(?s)^.""".r map (c => outcomb(c.head))
  def quesParse: EsoParser[Func] = "?" &> """(?s)^.""".r map (c => quescomb(c.head))
  def combParse: EsoParser[Func] = """^[ivkscdr@|e]""".r map {
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
  def junkParse: EsoParser[String] = """^[^ivkscdr@|e`.?#]*(?:#.*\v)?[^ivkscdr@|e`.?#]*""".r
  
  def appSParse: EsoParser[String] = ("`" &> (exprSParse <&> exprSParse).map{case (x, y) => s"`$x$y"}) map { s => println(s"Parsed App: $s"); s}
  def printSParse: EsoParser[String] = "." &> """(?s)^.""".r map { s => println(s"Parsed Out: .$s"); s".$s"}
  def quesSParse: EsoParser[String] = "?" &> """(?s)^.""".r map { s => println(s"Parsed   ?: ?$s"); s"?$s"}
  def combSParse: EsoParser[String] = """^[ivkscdr@|e]""".r map { s => println(s"Parsed Com: $s"); s}
  def exprSParse: EsoParser[String] = {
    (junkParse map {s => if(s.nonEmpty) println(s"""Parsed Jnk: "${s.replace("\n", "\\n")}""""); s}) &> (printSParse | quesSParse | combSParse | appSParse) map { s => println(s"Parsed Exp: ${s.replace(".\n", "r")}"); s}
  }
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    //println
    //exprSParse(progRaw)
    exprParse(progRaw).toTry("Invalid Unlambda Expression") map {prog =>
      inputs => LazyList.unfold(prog(endCont, Env(None, inputs)))(_.result.resolve)}
  }
  
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
  val dcomb: Func = (f, cc, env) => tailcall(cc(D1(funcExpr(f)), env))
}