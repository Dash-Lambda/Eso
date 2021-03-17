package lazybird

import common.{Config, Interpreter}
import parsers.EsoParser
import parsers.Implicits._

import scala.util.Try
import scala.util.control.TailCalls._

object LazyBird extends Interpreter{
  val name: String = "LazyBird"
  val sexpr: Expr = FE(S)
  val kexpr: Expr = FE(K)
  val iexpr: Expr = FE(I)
  
  def primParse: EsoParser[Expr] = "^.".r map {
    case "s" => sexpr
    case "k" => kexpr
    case "i" => iexpr
    case "m" => AE(AE(sexpr, iexpr), iexpr)
    case "0" => AE(kexpr, iexpr)
    case "w" => AE(AE(sexpr, sexpr), AE(kexpr, iexpr))
    case "u" => AE(AE(sexpr, AE(kexpr, AE(sexpr, iexpr))), AE(AE(sexpr, iexpr), iexpr))
    case "o" => AE(sexpr, iexpr)
    case "t" => AE(AE(sexpr, AE(kexpr, AE(sexpr, iexpr))), kexpr)
    case "l" => AE(AE(sexpr, AE(AE(sexpr, AE(kexpr, sexpr)), kexpr)), AE(kexpr, AE(AE(sexpr, iexpr), iexpr)))
    case "b" => AE(AE(sexpr, AE(kexpr, sexpr)), kexpr)
    case "c" => AE(AE(sexpr, AE(AE(sexpr, AE(kexpr, AE(AE(sexpr, AE(kexpr, sexpr)), kexpr))), sexpr)), AE(kexpr, kexpr))
    case "q" => AE(AE(sexpr, AE(kexpr, AE(sexpr, AE(AE(sexpr, AE(kexpr, sexpr)), kexpr)))), kexpr)
    case "v" => AE(AE(sexpr, AE(kexpr, AE(AE(sexpr, AE(AE(sexpr, AE(kexpr, AE(AE(sexpr, AE(kexpr, sexpr)), kexpr))), sexpr)), AE(kexpr, kexpr)))), AE(AE(sexpr, AE(kexpr, AE(sexpr, iexpr))), kexpr))
    case "@" => AE(AE(sexpr, AE(AE(sexpr, iexpr), AE(kexpr, sexpr))), AE(kexpr, kexpr))
    case "#" => FE(V)
    case "_" => FE(Read)
    case "&" => FE(Eql)
    case "r" => FE(PrintComb('\n'))
    case cl => CharExpr(cl.head)}
  def printParse: EsoParser[Expr] = "." &> "^.".r map (c => FE(PrintComb(c.head)))
  def absParse: EsoParser[Expr] = "^" &> ("^.".r <&> lzbParse) map {case (c, e) => e.elim(c.head.toLower)}
  def appParse: EsoParser[Expr] = "`" &> (lzbParse <&> lzbParse) map {case (x, y) => AppExpr(x, y)}
  def lzbParse: EsoParser[Expr] = appParse | absParse | printParse | primParse
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = lzbParse(progRaw).toTry() map{ prog =>
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
