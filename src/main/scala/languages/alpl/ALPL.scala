package languages.alpl

import common.{Config, Interpreter}
import parsers.EsoParser
import parsers.EsoParser._

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.Try
import scala.util.control.TailCalls._

object ALPL extends Interpreter{
  val name: String = "ALPL"
  
  def asnParse: EsoParser[Expr] = S("=") &> R("""^.""".r) map (s => FuncExpr(assign(s.head)))
  def prmParse: EsoParser[Expr] = R("""^[^\[\].`=]""".r) map {
    case "?" => FuncExpr(inpFunc)
    case "0" => FuncExpr(printFunc(false))
    case "1" => FuncExpr(printFunc(true))
    case str => RefExpr(str.head)}
  def lamParse: EsoParser[Expr] = (S("[") &> (R("""^[^.]*""".r) <&> (S(".") &> R("""^[^]]*""".r))) <& S("]")) map {
    case (p, f) =>
      val binds = p.toVector
      val ops = f.toVector.map{
        case '`' => None
        case c =>
          val expr = c match{
            case '?' => FuncExpr(inpFunc)
            case '0' => FuncExpr(printFunc(false))
            case '1' => FuncExpr(printFunc(true))
            case _ => binds.indexOf(c) match{
              case -1 => RefExpr(c)
              case i => BoundExpr(i)}}
          Some(expr)}
      FuncExpr(Lambda(ops, Vector(), binds.size))}
  def junkParse: EsoParser[String] = R("""^\s*""".r)
  def alplParse: EsoParser[Expr] = junkParse &> (asnParse | prmParse | lamParse | (S("`") &> (alplParse <&> alplParse) map {case (x, y) => AppExpr(x, y)}))
  def allParse: EsoParser[Vector[Expr]] = alplParse.*
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    allParse(progRaw).toTry() map{
      case (prog +: es, _, _, _) =>
        inputs => {
          val width = config.num("charWidth")
          val initEnv = Env(immutable.HashMap(), binInp(inputs, width), 0, width - 1)
          val initRet: TailRec[Ret] = tailcall(prog(passCont(es), initEnv))
          LazyList.unfold(initRet)(nxt => nxt.result.resolve)}}
  }
  
  def binInp(inp: Seq[Char], wid: Int = 8): Seq[Boolean] = {
    @tailrec
    def toBin(src: Int, ac: Vector[Boolean] = Vector()): Vector[Boolean] = {
      if(ac.sizeIs == wid) ac
      else toBin(src/2, (src%2 == 1) +: ac)}
    inp flatMap (c => toBin(c.toInt))}
  
  case class Env(funcs: immutable.HashMap[Char, Expr], inp: Seq[Boolean], oac: Int, onum: Int){
    def apply(c: Char): Expr = funcs(c)
    def add(c: Char, exp: Expr): Env = Env(funcs + ((c, exp)), inp, oac, onum)
    def read: (Boolean, Env) = (inp.head, Env(funcs, inp.tail, oac, onum))
    def write(bit: Int): (Option[Char], Env) = {
      val nac = (oac*2) + bit
      if(onum == 0) (Some(nac.toChar), Env(funcs, inp, 0, 7))
      else (None, Env(funcs, inp, nac, onum - 1))}
    def head: Boolean = inp.head
    def tail: Env = Env(funcs, inp.tail, oac, onum)}
  
  trait Ret{
    def resolve: Option[(Char, TailRec[Ret])]}
  abstract class Expr extends ((Cont, Env) => TailRec[Ret])
  abstract class Cont extends ((Func, Env) => TailRec[Ret])
  abstract class Func extends ((Expr, Cont, Env) => TailRec[Ret])
  
  //Returns
  object HaltRet extends Ret{
    def resolve: Option[(Char, TailRec[Ret])] = None}
  
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
    def apply(cc: Cont, env: Env): TailRec[Ret] = tailcall(x(exprCont(y, cc), env))}
  
  //Continuations
  def passCont(exps: Seq[Expr]): Cont = {
    (_, env) => exps match{
      case e +: es => tailcall(e(passCont(es), env))
      case _ => done(HaltRet)}}
  def exprCont(y: Expr, cc: Cont): Cont = (f, env) => tailcall(f(y, cc, env))
  
  //Functions
  def assign(c: Char): Func = (f, cc, env) => tailcall(f(cc, env.add(c, f)))
  def inpFunc: Func = (x, c1, en1) => c1((y, c2, en2) => if(en2.head) tailcall(x(c2, en2.tail)) else tailcall(y(c2, en2.tail)), en1)
  def printFunc(bit: Boolean): Func = {
    (f, cc, env) => env.write(if(bit) 1 else 0) match{
      case (res, nenv) => res match{
        case Some(c) => done(PrintRet(c, tailcall(f(cc, nenv))))
        case None => tailcall(f(cc, nenv))}}}
  
  case class Lambda(ops: Vector[Option[Expr]], bound: Vector[Expr], rem: Int) extends Func{
    def apply(f: Expr, cc: Cont, env: Env): TailRec[Ret] = {
      if(rem == 1) tailcall(collapse(bound :+ f)(cc, env))
      else tailcall(cc(Lambda(ops, bound :+ f, rem - 1), env))}
    
    def collapse(binds: Vector[Expr]): Expr = {
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
          case Some(BoundExpr(n)) => cdo(es, cc(binds(n)))
          case Some(ex) => cdo(es, cc(ex))
          case None => cdo(es, APC(cc))}
        case _ => cc match{
          case RPC(res) => res}}
      cdo(ops, EPC)}}
}
