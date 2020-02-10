package lazyk

import common.{Config, EsoExcep, Interpreter}
import parsers.{ARPDown, ARPFail, ARPNext, ARPRet, ARPUp, ArbitraryRecurParser, ChunkParser, EsoParser, PartialParser, DepthRecurParser}

import scala.annotation.tailrec
import scala.util.{Failure, Try}
import scala.util.control.TailCalls._

object LazyK extends Interpreter{
  val name: String = "LazyK"
  
  val churchTrue: Func = K
  val churchFalse: Func = K1(FuncExpr(I))
  
  val lkParser: EsoParser[Seq[Char], Expr] = {
    val sexp: Expr = FuncExpr(S)
    val kexp: Expr = FuncExpr(K)
    val iexp: Expr = FuncExpr(I)
    val iotaexp: Expr = FuncExpr(Pair(sexp, kexp))
    
    val unlParser: EsoParser[Seq[Char], Expr] = {
      val funcParser = {
        PartialParser[Seq[Char], Expr]{
          case 's' +: cs => (sexp, cs, 0, 1)
          case 'k' +: cs => (kexp, cs, 0, 1)
          case 'i' +: cs => (iexp, cs, 0, 1)}}
      def recur(src: Seq[Char]): Option[(Seq[Char], Int, Int)] = src match{
        case '`' +: cs => Some((cs, 0, 1))
        case _ => None}
      def collect(xs: Seq[Expr]): Expr = xs match{
        case x +: y +: _ => AppExpr(x, y)}
      DepthRecurParser(2)(recur)(collect)(funcParser)}
    
    val combParser: EsoParser[Seq[Char], Expr] = {
      def collect(src: Seq[Expr]): Expr = src.foldLeft(iexp)(AppExpr)
      def recur(src: Seq[Char]): ARPRet[Seq[Char], Expr] = src match{
        case c +: cs => c match{
          case '(' => ARPDown(cs, 0, 1)
          case ')' => ARPUp(cs, 0, 1)
          case 'S' => ARPNext(sexp, cs, 0, 1)
          case 'K' => ARPNext(kexp, cs, 0, 1)
          case 'I' => ARPNext(iexp, cs, 0, 1)
          case _ => ARPFail}
        case _ => ARPUp(src, 0, 0)}
      ArbitraryRecurParser(recur _, collect)}
    
    val iotaParser: EsoParser[Seq[Char], Expr] = {
      val funcParser = {
        PartialParser[Seq[Char], Expr]{
          case 'i' +: cs => (iotaexp, cs, 0, 1)}}
      def recur(src: Seq[Char]): Option[(Seq[Char], Int, Int)] = src match{
        case '*' +: cs => Some((cs, 0, 1))
        case _ => None}
      def collect(xs: Seq[Expr]): Expr = xs match{
        case x +: y +: _ => AppExpr(x, y)}
      DepthRecurParser(2)(recur)(collect)(funcParser)}
    
    val jotParser: EsoParser[Seq[Char], Expr] = {
      def collect(exps: Vector[Expr]): Expr = exps.foldLeft(iexp){case (x, y) => AppExpr(x, y)}
      @tailrec
      def jdo(src: Seq[Char], ac: Vector[Expr]): (Expr, Seq[Char]) = src match{
        case w :+ j => j match{
          case '0' => jdo(w, sexp +: kexp +: ac)
          case '1' => ac match{
            case x +: y +: es => jdo(w, AppExpr(x, y) +: es)}
          case _ => (collect(ac), src)}
        case _ => (collect(ac), src)}
      ChunkParser[Seq[Char], Expr]{ inp =>
        val end = inp.lastIndexWhere("01".contains(_))
        if(end == -1) None
        else jdo(inp.take(end + 1), Vector()) match{
          case (res, rem) => Some((res, rem, rem.size - 1, end + 1))}}}
    
    (unlParser <+> combParser <+> iotaParser <+> jotParser)
      .withConditioning(p => filterContains(p.toVector, "*`ski(SKI)01"))}
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = parse(progRaw) match{
    case None => Failure(EsoExcep("Invalid Expression"))
    case Some(initExpr) => Try{
      inputs =>
        val cinp = ChurchList(inputs.to(LazyList).map(_.toInt).takeWhile(_ <= 255) #::: LazyList.continually(256))
        val fexpr: Expr = AppExpr(initExpr, FuncExpr(cinp))
        LazyList.unfold(fexpr)(run)}}
  
  def parse(progRaw: String): Option[Expr] = {
    def uncomment(str: String): String = str.replaceAll("""(?m)^#.*$""", "")
    lkParser(uncomment(progRaw)).toOption}
  
  def run(expr: Expr): Option[(Char, Expr)] = {
    val cur = FuncExpr(eval(expr))
    val cexpr = {
      AppExpr(
        AppExpr(
          AppExpr(cur, FuncExpr(churchTrue)),
          FuncExpr(ChurchCounter)),
        FuncExpr(ChurchHalt))}
    val num = countChurchNum(eval(cexpr))
    if(0 > num || num > 255) None
    else{
      val nexpr = AppExpr(cur, FuncExpr(churchFalse))
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
  
  class DupExpr(var f: Expr) extends Expr{
    def apply(cc: Cont): TailRec[Func] = f match{
      case FuncExpr(fun) => tailcall(cc(fun))
      case _ => tailcall(f(DupCont(this, cc)))}
    def collapse(res: Func): Unit = f match{
      case FuncExpr(_) => ()
      case _ => f = FuncExpr(res)}}
  
  //Continuations
  object EndCont extends Cont{
    def apply(f: Func): TailRec[Func] = done(f)}
  
  case class ExprCont(y: Expr, cc: Cont) extends Cont{
    def apply(f: Func): TailRec[Func] = tailcall(f(y, cc))}
  
  case class DupCont(u: DupExpr, cc: Cont) extends Cont{
    def apply(f: Func): TailRec[Func] = {
      u.collapse(f)
      tailcall(cc(f))}}
  
  //Funcs
  object I extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(f(cc))}
  
  case class K1(x: Expr) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(x(cc))}
  object K extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = tailcall(cc(K1(f)))}
  
  case class S2(x: Expr, y: Expr) extends Func{
    def apply(f: Expr, cc: Cont): TailRec[Func] = {
      val fex = new DupExpr(f)
      tailcall(x(ExprCont(fex, ExprCont(AppExpr(y, fex), cc))))}}
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