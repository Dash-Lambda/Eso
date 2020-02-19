package alpl

import common.{Config, Interpreter}
import parsers.{DepthRecurParser, RegexParser}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.Try
import scala.util.control.TailCalls._

object ALPL extends Interpreter{
  val name: String = "ALPL"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = Try{parse(progRaw)} map{prog =>
    inputs => {
      val initEnv = Env(immutable.HashMap(), binInp(inputs), 0, 0)
      val initExpr: TailRec[Ret] = prog(EndCont, initEnv)
      LazyList.unfold(initExpr)(nxt => nxt.result.resolve)}}
  
  def binInp(inp: Seq[Char]): Seq[Boolean] = inp flatMap (c => c.toBinaryString.toVector.map(_ == '1'))
  
  def parse(progRaw: String): Expr = {
    val asnParser = RegexParser(raw"""=(.)""")(m => FuncExpr(Assign(m.group(1).head)))
    val prmParser = RegexParser(raw"""([\?01])"""){m =>
      m.group(1) match{
        case "?" => FuncExpr(Inp)
        case "0" => FuncExpr(Print(false))
        case "1" => FuncExpr(Print(true))}}
    val lamParser = RegexParser(raw"""\[([^\.]*)\.([^\]]*)\]"""){m =>
      val binds = m.group(1).toVector
      val ops = m.group(2).toVector.map{
        case '`' => None
        case c =>
          val expr = c match{
            case '?' => FuncExpr(Inp)
            case '0' => FuncExpr(Print(false))
            case '1' => FuncExpr(Print(true))
            case _ => binds.indexOf(c) match{
              case -1 => RefExpr(c)
              case i => BoundExpr(i)}}
          Some(expr)}
      FuncExpr(Lambda(ops, Vector(), binds.size))}
    val termParser = asnParser <+> prmParser <+> lamParser
    
    val recReg = raw"""`(.*)""".r
    def recur(src: String): Option[(String, Int, Int)] = src match{
      case recReg(rem) => Some((rem, 0, 1))
      case _ => None}
    def collect(xs: Seq[Expr]): Expr = xs match{
      case x +: y +: _ => AppExpr(x, y)}
    
    val allParser = DepthRecurParser(2)(recur)(collect)(termParser)
    
    allParser.parseOne(progRaw)}
  
  case class Env(funcs: immutable.HashMap[Char, Expr], inp: Seq[Boolean], oac: Int, onum: Int){
    def apply(c: Char): Expr = funcs(c)
    def add(c: Char, exp: Expr): Env = Env(funcs + ((c, exp)), inp, oac, onum)
    def read: (Boolean, Env) = (inp.head, Env(funcs, inp.tail, oac, onum))
    def write(bit: Int): (Option[Char], Env) = {
      val nac = (oac*2) + bit
      if(onum == 0) (Some(nac.toChar), Env(funcs, inp, 0, 7))
      else (None, Env(funcs, inp, nac, onum - 1))}}
  
  trait Ret{
    def resolve: Option[(Char, TailRec[Ret])]}
  
  trait Expr{
    def apply(cc: Cont, env: Env): TailRec[Ret]}
  
  trait Cont{
    def apply(f: Func, env: Env): TailRec[Ret]}
  
  trait Func{
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret]}
  
  //Returns
  object HaltRet extends Ret{
    def resolve: Option[(Char, TailRec[Ret])] = None
  }
  case class PrintRet(c: Char, nxt: TailRec[Ret]) extends Ret {
    def resolve: Option[(Char, TailRec[Ret])] = Some((c, nxt))}
  
  //Expressions
  case class FuncExpr(f: Func) extends Expr{
    def apply(cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(f, env))}
  
  case class RefExpr(c: Char) extends Expr{
    def apply(cc: Cont, env: Env): TailRec[Ret] = tailcall(env(c)(cc, env))}
  
  case class BoundExpr(n: Int) extends Expr{
    def apply(cc: Cont, env: Env): TailRec[Ret] = done(HaltRet)}
  
  case class AppExpr(x: Expr, y: Expr) extends Expr{
    def apply(cc: Cont, env: Env): TailRec[Ret] = tailcall(x(ExprCont(y, cc), env))}
  
  //Continuations
  object EndCont extends Cont{
    def apply(f: Func, env: Env): TailRec[Ret] = done(HaltRet)}
  
  case class ExprCont(y: Expr, cc: Cont) extends Cont{
    def apply(f: Func, env: Env): TailRec[Ret] = tailcall(f(y, cc, env))}
  
  //Functions
  case class Inp1(x: Expr) extends Func{
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = env.read match{
      case (bit, nenv) =>
        if(bit) tailcall(x(cc, nenv))
        else tailcall(f(cc, nenv))}}
  object Inp extends Func{
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = tailcall(cc(Inp1(f), env))}
  
  case class Print(bit: Boolean) extends Func{
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = env.write(if(bit) 1 else 0) match{
      case (res, nenv) => res match{
        case Some(c) => done(PrintRet(c, tailcall(f(cc, nenv))))
        case None => tailcall(f(cc, nenv))}}}
  
  case class Assign(c: Char) extends Func{
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = tailcall(f(cc, env.add(c, f)))}
  
  case class Lambda(ops: Vector[Option[Expr]], bound: Vector[Expr], rem: Int) extends Func{
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = {
      if(rem == 0) tailcall(AppExpr(collapse, f)(cc, env))
      else tailcall(cc(Lambda(ops, bound :+ f, rem - 1), env))}
    
    def collapse: Expr = {
      trait PC{
        def apply(ex: Expr): PC}
      object EPC extends PC{
        def apply(ex: Expr): PC = RPC(ex)}
      case class RPC(res: Expr) extends PC{
        def apply(ex: Expr): PC = RPC(ex)}
      case class APC(cc: PC) extends PC{
        def apply(ex: Expr): PC = APC1(ex, cc)}
      case class APC1(x: Expr, cc: PC) extends PC{
        def apply(ex: Expr): PC = cc(AppExpr(x, ex))}
      
      @tailrec
      def cdo(src: Vector[Option[Expr]], cc: PC): Expr = src match{
        case e +: es => e match{
          case Some(BoundExpr(n)) => cdo(es, cc(bound(n)))
          case Some(ex) => cdo(es, cc(ex))
          case None => cdo(es, APC(cc))}
        case _ => cc match{
          case RPC(res) => res}}
      cdo(ops, EPC)}
  }
}
