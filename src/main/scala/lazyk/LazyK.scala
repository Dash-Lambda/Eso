package lazyk

import common.{Config, EsoExcep, Interpreter}
import parsers.{ARPDown, ARPFail, ARPNext, ARPRet, ARPUp, ArbitraryRecurParser, OrderedParser, OrderedPartialParser, OrderedRecurParser}

import scala.annotation.tailrec
import scala.util.{Failure, Try}
import scala.util.control.TailCalls._

object LazyK extends Interpreter{
  val name: String = "LazyK"
  
  val sexp: Expr = FuncExpr(S)
  val kexp: Expr = FuncExpr(K)
  val iexp: Expr = FuncExpr(I)
  
  val churchTrue: Func = K
  val churchFalse: Func = K1(iexp)
  
  val unlParser: OrderedParser[Vector[Char], Expr] = {
    val funcParser = {
      OrderedPartialParser[Vector[Char], Expr]{
        case 's' +: cs => (sexp, cs, 0, 1)
        case 'k' +: cs => (kexp, cs, 0, 1)
        case 'i' +: cs => (iexp, cs, 0, 1)}}
    def recur(src: Vector[Char]): Option[(Vector[Char], Int, Int)] = src match{
      case '`' +: cs => Some((cs, 0, 1))
      case _ => None}
    def collect(xs: Seq[Expr]): Expr = xs match{
      case x +: y +: _ => AppExpr(x, y)}
    OrderedRecurParser(2)(recur)(collect)(funcParser)}
  val combParser: OrderedParser[Vector[Char], Expr] = {
    def collect(src: Seq[Expr]): Expr = src.reduceLeft(AppExpr)
    def recur(src: Vector[Char]): ARPRet[Vector[Char], Expr] = src match{
      case c +: cs => c match{
        case '(' => ARPDown(cs, 0, 1)
        case ')' => ARPUp(cs, 0, 1)
        case 'S' => ARPNext(sexp, cs, 0, 1)
        case 'K' => ARPNext(kexp, cs, 0, 1)
        case 'I' => ARPNext(iexp, cs, 0, 1)
        case _ => ARPFail}
      case _ => ARPUp(src, 0, 0)}
    ArbitraryRecurParser(recur _, collect)}
  val iotaParser: OrderedParser[Vector[Char], Expr] = {
    val funcParser = {
      OrderedPartialParser[Vector[Char], Expr]{
        case 'i' +: cs => (FuncExpr(Pair(sexp, kexp)), cs, 0, 1)}}
    def recur(src: Vector[Char]): Option[(Vector[Char], Int, Int)] = src match{
      case '*' +: cs => Some((cs, 0, 1))
      case _ => None}
    def collect(xs: Seq[Expr]): Expr = xs match{
      case x +: y +: _ => AppExpr(x, y)}
    OrderedRecurParser(2)(recur)(collect)(funcParser)}
  val totParser: OrderedParser[Vector[Char], Expr] = unlParser <+> combParser <+> iotaParser
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = parse(progRaw) match{
    case None => Failure(EsoExcep("Invalid Expression"))
    case Some(initExpr) => Try{
      inputs =>
        val cinp = ChurchList(inputs.to(LazyList).map(_.toInt).takeWhile(_ <= 255) #::: LazyList.continually(256))
        val fexpr: Expr = AppExpr(initExpr, FuncExpr(cinp))
        LazyList.unfold(fexpr)(run)}}
  
  def parse(progRaw: String): Option[Expr] = {
    def uncomment(str: String): String = str.replaceAll("""(?m)^#.*$""", "")
    val conditioned = filterChars(uncomment(progRaw), "`ski(SKI)01*").toVector
    totParser(conditioned).toOption}
  
  def run(expr: Expr): Option[(Char, Expr)] = {
    val cur = eval(expr)
    val cexpr = {
      AppExpr(
        AppExpr(
          AppExpr(
            FuncExpr(cur),
            FuncExpr(churchTrue)),
          FuncExpr(ChurchCounter)),
        FuncExpr(ChurchHalt))}
    val num = countChurchNum(eval(cexpr))
    if(0 > num || num > 255) None
    else{
      val nexpr = {
        AppExpr(
          FuncExpr(cur),
          FuncExpr(churchFalse))}
      Some((num.toChar, nexpr))}}
  
  @tailrec
  def countChurchNum(fun: Func, ac: Int = 0): Int = {
    fun match{
      case ChurchReturn(nexp) => countChurchNum(eval(nexp), ac + 1)
      case ChurchHalt => ac
      case _ => -1}}
  
  def eval(expr: Expr): Func = expr(EndCont).result
  
  trait Expr{
    def apply(cc: Cont): TailRec[Func]}
  
  trait Cont{
    def apply(f: Func): TailRec[Func]}
  
  trait Func{
    def apply(f: Expr, cc: Cont): TailRec[Func]}
  
  //Expressions
  case class FuncExpr(f: Func) extends Expr{
    def apply(cc: Cont): TailRec[Func] = tailcall(cc(f))}
  
  case class AppExpr(x: Expr, y: Expr) extends Expr{
    def apply(cc: Cont): TailRec[Func] = tailcall(x(ExprCont(y, cc)))}
  
  //Continuations
  object EndCont extends Cont{
    def apply(f: Func): TailRec[Func] = done(f)}
  
  case class ExprCont(y: Expr, cc: Cont) extends Cont{
    def apply(f: Func): TailRec[Func] = tailcall(f(y, cc))}
  
  //Funcs
  object I extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(f(cc))}
  
  case class K1(x: Expr) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(x(cc))}
  object K extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(cc(K1(f)))}
  
  case class S2(x: Expr, y: Expr) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall{
      x(ExprCont(f, ExprCont(AppExpr(y, f), cc)))}}
  case class S1(x: Expr) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(cc(S2(x, f)))}
  object S extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(cc(S1(f)))}
  
  case class ChurchNum1(num: Int, fexp: Expr) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall{
      Iterator.fill(num)(fexp).foldLeft(f){case (y, x) => AppExpr(x, y)}(cc)}}
  case class ChurchNum(num: Int) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(cc(ChurchNum1(num, f)))}
  
  case class Pair(x: Expr, y: Expr) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(f(ExprCont(x, ExprCont(y, cc))))}
  
  case class ChurchList(lst: Seq[Int]) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = lst match{
      case n +: ns => tailcall(Pair(FuncExpr(ChurchNum(n)), FuncExpr(ChurchList(ns)))(f, cc))}}
  
  object ChurchHalt extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = done(ChurchCounter)}
  case class ChurchReturn(exp: Expr) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = done(this)}
  object ChurchCounter extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = done(ChurchReturn(f))}
}