package grass

import common.{Config, Interpreter}
import parsers.EsoParser
import parsers.Implicits._

import scala.annotation.tailrec
import scala.util.Try

object Grass extends Interpreter{
  val name: String = "Grass"
  @tailrec
  def absArity(n: Int, p: Vector[Expr]): Vector[Expr] = {
    if(n > 0) absArity(n - 1, Vector(AbsExpr(p)))
    else p}
  def firstParse: EsoParser[String] = "(?s)[ｗw].*".r map (s =>
    filterChars(s, "\uff37\uff57\uFF56\uFF36WwVv")
      .replace("\uff37", "W")
      .replace("\uff57", "w")
      .replace("\uFF56", "v")
      .replace("\uFF36", "v"))
  def appsParse: EsoParser[Vector[Expr]] = (("^W+".r <&> "^w+".r) map {case (x, y) => AppExpr(x.length, y.length)}).*
  def abstractParse: EsoParser[Vector[Expr]] = "^w[Ww]*".r >> ("w".all flatMap (v => appsParse.map{ es => absArity(v.length, es)}))
  def applyParse: EsoParser[Vector[Expr]] = "^W[Ww]*".r >> appsParse
  def grassParse: EsoParser[Vector[Expr]] = firstParse >> {
    ("^v?".r &> (abstractParse | applyParse)).all map {res =>
      res.flatten match{
        case as :+ (a: AbsExpr) => as :+ a :+ AppExpr(1, 1)
        case as => as}}}
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = Try{eval(grassParse.parseOne(progRaw))}
  
  def eval(prog: Vector[Expr]): Seq[Char] => LazyList[Char] = {
    @tailrec
    def edo(state: State): Option[(Char, State)] = state.run() match {
      case OutState(c, nxt) => Some((c, nxt))
      case HaltState => None
      case _ => edo(state.run())}
    inputs => LazyList.unfold(RunState(prog, Env(Vector(Out, Succ, CharFun('w'), In)), inputs, HaltState): State)(edo)}
  
  case class Env(stk: Vector[Func]){
    def apply(i: Int): Func = stk(i)
    def head: Func = stk.head
    def push(f: Func): Env = Env(f +: stk)}
  
  trait State{
    def run(): State
    def pass(res: Func): State}
  
  case class RunState(code: Vector[Expr], env: Env, inp: Seq[Char], call: State) extends State{
    def run(): State = code match{
      case a +: as => a(RunState(as, env, inp, call))
      case _ => call.pass(env.head)}
    def pass(res: Func): State = RunState(code, env.push(res), inp, call)}
  
  case class OutState(c: Char, nxt: State) extends State{
    def run(): State = nxt
    def pass(res: Func): State = OutState(c, nxt)}
  
  object HaltState extends State{
    def run(): State = HaltState
    def pass(res: Func): State = HaltState}
  
  trait Expr{
    def apply(ce: RunState): State}
  
  case class AppExpr(fi: Int, ai: Int) extends Expr{
    def apply(ce: RunState): State = ce.env(fi - 1)(ce.env(ai - 1), ce)}
  
  case class AbsExpr(apps: Vector[Expr]) extends Expr{
    def apply(ce: RunState): State = RunState(ce.code, ce.env.push(Closure(ce.env, apps)), ce.inp, ce.call)}
  
  trait Func{
    def apply(arg: Func, ce: RunState): State}
  
  case class Closure(env: Env, code: Vector[Expr]) extends Func{
    def apply(arg: Func, ce: RunState): State = RunState(code, env.push(arg), ce.inp, ce)}
  
  object Ident extends Func{
    def apply(arg: Func, ce: RunState): State = RunState(ce.code, ce.env.push(arg), ce.inp, ce.call)}
  
  case class Const(f: Func) extends Func{
    def apply(arg: Func, ce: RunState): State = RunState(ce.code, ce.env.push(f), ce.inp, ce.call)}
  
  object CTrue extends Func{
    def apply(arg: Func, ce: RunState): State = RunState(ce.code, ce.env.push(Const(arg)), ce.inp, ce.call)}
  
  object CFalse extends Func{
    def apply(arg: Func, ce: RunState): State = RunState(ce.code, ce.env.push(Ident), ce.inp, ce.call)}
  
  case class CharFun(c: Char) extends Func{
    def apply(arg: Func, ce: RunState): State = {
      val f = arg match{
        case CharFun(a) if a == c => CTrue
        case _ => CFalse}
      RunState(ce.code, ce.env.push(f), ce.inp, ce.call)}}
  
  object Out extends Func{
    def apply(arg: Func, ce: RunState): State = arg match{
      case CharFun(c) => OutState(c, RunState(ce.code, ce.env.push(arg), ce.inp, ce.call))}}
  
  object In extends Func{
    def apply(arg: Func, ce: RunState): State = {
      RunState(ce.code, ce.env.push(CharFun(ce.inp.head)), ce.inp.tail, ce.call)}}
  
  object Succ extends Func{
    def apply(arg: Func, ce: RunState): State = arg match{
      case CharFun(c) => RunState(ce.code, ce.env.push(CharFun(((c.toInt + 1)%256).toChar)), ce.inp, ce.call)}}
}
