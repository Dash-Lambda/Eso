package unlambda

import common.{Config, EsoExcep, Interpreter}
import parsers.{EsoParsed, EsoParser, PartialParser, DepthRecurParser}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.{Failure, Success, Try}

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
    def recur(src: Vector[Char]): Option[(Vector[Char], Int, Int)] = src match{
      case '`' +: cs => Some((cs, 0, 1))
      case _ => None}
    def collect(xs: Seq[Expr]): Expr = xs match{
      case x +: y +: _ => AppExpr(x, y)}
    DepthRecurParser(funcParser)(2)(recur)(collect)}
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    @tailrec
    def udo(state: State): Option[(Char, State)] = state match{
      case EndState => None
      case PrintState(c, nxt) => Some((c, nxt))
      case _ => udo(state.step())}
    parse(progRaw) map {prog =>
      inputs => LazyList.unfold(EvalState(prog, EndCont, Env(None, inputs)): State)(udo)}}
  
  def parse(progRaw: String): Try[Expr] = {
    val toks = "ivkscdr@|e`.?#".toVector
    @tailrec
    def condition(src: Vector[Char], ac: Vector[Char] = Vector()): Vector[Char] = src match{
      case '.' +: c +: cs => condition(cs, ac :+ '.' :+ c)
      case '?' +: c +: cs => condition(cs, ac :+ '?' :+ c)
      case '#' +: cs => condition(cs.dropWhile(_ != '\n'), ac)
      case c +: cs if toks.contains(c) => condition(cs, ac :+ c)
      case _ +: cs => condition(cs, ac)
      case _ => ac}
    
    unlParser(condition(progRaw.toVector)) match{
      case EsoParsed(prog, _, _, _) => Success(prog)
      case _ => Failure(EsoExcep("Invalid Unlambda Expression"))}}
  
  case class Env(cur: Option[Char], inp: Seq[Char]){
    def read: Env = inp match{
      case c +: cs => Env(Some(c), cs)
      case _ => Env(None, inp)}}
  
  trait State{
    def step(): State}
  
  trait Expr{
    def apply(cc: Cont, env: Env): State}
  
  trait Cont{
    def apply(f: Func, env: Env): State}
  
  trait Func{
    def apply(f: Func, cc: Cont, env: Env): State}
  
  //States
  object EndState extends State{
    def step(): State = EndState}
  
  case class EvalState(x: Expr, cc: Cont, env: Env) extends State{
    def step(): State = x(cc, env)}
  
  case class AppState(x: Func, y: Func, cc: Cont, env: Env) extends State{
    def step(): State = x(y, cc, env)}
  
  case class PrintState(c: Char, nxt: State) extends State{
    def step(): State = nxt}
  
  //Expressions
  case class FuncExpr(f: Func) extends Expr{
    def apply(cc: Cont, env: Env): State = cc(f, env)}
  
  case class AppExpr(x: Expr, y: Expr) extends Expr{
    def apply(cc: Cont, env: Env): State = EvalState(x, ExprCont(y, cc), env)}
  
  //Continuations
  object EndCont extends Cont{
    def apply(f: Func, env: Env): State = EndState}
  
  case class FuncCont(x: Func, cc: Cont) extends Cont{
    def apply(f: Func, env: Env): State = AppState(x, f, cc, env)}
  
  case class ExprCont(y: Expr, cc: Cont) extends Cont{
    def apply(f: Func, env: Env): State = f match{
      case D => cc(D1(y), env)
      case _ => EvalState(y, FuncCont(f, cc), env)}}
  
  case class DCont(y: Func, cc: Cont) extends Cont{
    def apply(f: Func, env: Env): State = AppState(f, y, cc, env)}
  
  //Functions
  object I extends Func{
    def apply(f: Func, cc: Cont, env: Env): State = cc(f, env)}
  
  object V extends Func{
    def apply(f: Func, cc: Cont, env: Env): State = cc(V, env)}
  
  case class OUT(c: Char) extends Func{
    def apply(f: Func, cc: Cont, env: Env): State = PrintState(c, cc(f, env))}
  
  case class D1(x: Expr) extends Func{
    def apply(f: Func, cc: Cont, env: Env): State = EvalState(x, DCont(f, cc), env)}
  
  object D extends Func{
    def apply(f: Func, cc: Cont, env: Env): State = cc(D1(FuncExpr(f)), env)}
  
  case class S2(x: Func, y: Func) extends Func{
    def apply(f: Func, cc: Cont, env: Env): State = {
      EvalState(
        AppExpr(
          AppExpr(
            FuncExpr(x),
            FuncExpr(f)),
          AppExpr(
            FuncExpr(y),
            FuncExpr(f))),
        cc, env)}}
  case class S1(x: Func) extends Func{
    def apply(f: Func, cc: Cont, env: Env): State = cc(S2(x, f), env)}
  object S extends Func{
    def apply(f: Func, cc: Cont, env: Env): State = cc(S1(f), env)}
  
  case class K1(x: Func) extends Func{
    def apply(f: Func, cc: Cont, env: Env): State = cc(x, env)}
  object K extends Func{
    def apply(f: Func, cc: Cont, env: Env): State = cc(K1(f), env)}
  
  case class C1(cc1: Cont) extends Func{
    def apply(f: Func, cc: Cont, env: Env): State = cc1(f, env)}
  object C extends Func{
    def apply(f: Func, cc: Cont, env: Env): State = AppState(f, C1(cc), cc, env)}
  
  object E extends Func{
    def apply(f: Func, cc: Cont, env: Env): State = EndState}
  
  object AT extends Func{
    def apply(f: Func, cc: Cont, env: Env): State = env.cur match{
      case None => AppState(f, V, cc, env.read)
      case _ => AppState(f, I, cc, env.read)}}
  
  case class QUES(c: Char) extends Func{
    def apply(f: Func, cc: Cont, env: Env): State = env.cur match{
      case Some(`c`) => AppState(f, I, cc, env)
      case _ => AppState(f, V, cc, env)}}
  
  object PIPE extends Func{
    def apply(f: Func, cc: Cont, env: Env): State = env.cur match{
      case Some(c) => AppState(f, OUT(c), cc, env)
      case None => AppState(f, V, cc, env)}}
}