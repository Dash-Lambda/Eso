package lazyk

import common.EsoObj
import lazyk.LazyKFuncs._
import parsers.{ARPDown, ARPFail, ARPNext, ARPRet, ARPUp, ArbitraryRecurParser, DepthRecurParser, EsoParseFail, EsoParsed, EsoParser, PartialParser}

import scala.annotation.tailrec

object LazyKParsers extends EsoObj{
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
    DepthRecurParser(funcParser)(2)(recur)(collect)}
  
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
    ArbitraryRecurParser(recur _)(collect)}
  
  val iotaParser: EsoParser[Seq[Char], Expr] = {
    val funcParser = {
      PartialParser[Seq[Char], Expr]{
        case 'i' +: cs => (iotaexp, cs, 0, 1)}}
    def recur(src: Seq[Char]): Option[(Seq[Char], Int, Int)] = src match{
      case '*' +: cs => Some((cs, 0, 1))
      case _ => None}
    def collect(xs: Seq[Expr]): Expr = xs match{
      case x +: y +: _ => AppExpr(x, y)}
    DepthRecurParser(funcParser)(2)(recur)(collect)}
  
  val jotParser: EsoParser[Seq[Char], Expr] = (inp: Seq[Char]) => {
    def collect(exps: Vector[Expr]): Expr = exps.foldLeft(iexp){case (x, y) => AppExpr(x, y)}
    @tailrec
    def jdo(src: Seq[Char], ac: Vector[Expr]): (Expr, Seq[Char]) = src match{
      case w :+ j => j match{
        case '0' => jdo(w, sexp +: kexp +: ac)
        case '1' => ac match{
          case x +: y +: es => jdo(w, AppExpr(x, y) +: es)}
        case _ => (collect(ac), src)}
      case _ => (collect(ac), src)}
    val end = inp.lastIndexWhere("01".contains(_))
    if (end == -1) EsoParseFail
    else jdo(inp.take(end + 1), Vector()) match{
      case (res, rem) => EsoParsed(res, rem, rem.size - 1, end + 1)}}
  
  val lkParser: EsoParser[Seq[Char], Expr] = {
    (unlParser <+> combParser <+> iotaParser <+> jotParser)
      .withConditioning(p => filterContains(p.toVector, "*`ski(SKI)01"))}
  
  def parse(progRaw: String): Option[Expr] = {
    def uncomment(str: String): String = str.replaceAll("""(?m)^#.*$""", "")
    lkParser(uncomment(progRaw)).toOption}
}
