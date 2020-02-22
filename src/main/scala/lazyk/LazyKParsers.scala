package lazyk

import common.EsoObj
import lazyk.LazyKFuncs._
import parsers.{ARPDown, ARPFail, ARPNext, ARPRet, ARPUp, ArbitraryRecurParser, DepthRecurParser, EsoParseFail, EsoParsed, EsoParser, PartialElementwiseParser}

import scala.annotation.tailrec
import scala.util.Try

object LazyKParsers extends EsoObj{
  val iotaexp: Expr = churchPair(scomb, kcomb)
  
  val unlParser: EsoParser[Seq[Char], Expr] = {
    val funcParser = PartialElementwiseParser[Char, Expr]{
      case 's' => scomb
      case 'k' => kcomb
      case 'i' => icomb}
    def recur(src: Seq[Char]): Option[(Seq[Char], Int, Int)] = src match{
      case '`' +: cs => Some((cs, 0, 1))
      case _ => None}
    def collect(xs: Seq[Expr]): Expr = xs.reduceLeft(AppExpr)
    DepthRecurParser(funcParser)(2)(recur)(collect)}
  
  val combParser: EsoParser[Seq[Char], Expr] = {
    def collect(src: Seq[Expr]): Expr = src.reduceLeft(AppExpr)
    def recur(src: Seq[Char]): ARPRet[Seq[Char], Expr] = src match{
      case c +: cs => c match{
        case '(' => ARPDown(cs, 0, 1)
        case ')' => ARPUp(cs, 0, 1)
        case 'S' => ARPNext(scomb, cs, 0, 1)
        case 'K' => ARPNext(kcomb, cs, 0, 1)
        case 'I' => ARPNext(icomb, cs, 0, 1)
        case _ => ARPFail}
      case _ => ARPUp(src, 0, 0)}
    ArbitraryRecurParser(recur _)(collect)}
  
  val iotaParser: EsoParser[Seq[Char], Expr] = {
    def recur(src: Seq[Char]): Option[(Seq[Char], Int, Int)] = src match{
      case '*' +: cs => Some((cs, 0, 1))
      case _ => None}
    def collect(xs: Seq[Expr]): Expr = xs.reduceLeft(AppExpr)
    DepthRecurParser(PartialElementwiseParser[Char, Expr]{case 'i' => iotaexp})(2)(recur)(collect)}
  
  val jotParser: EsoParser[Seq[Char], Expr] = inp => {
    def collect(exps: Vector[Expr]): Expr = exps.foldLeft(icomb: Expr){case (x, y) => AppExpr(x, y)}
    @tailrec
    def jdo(src: Seq[Char], ac: Vector[Expr]): (Expr, Seq[Char]) = src match{
      case w :+ j => j match{
        case '0' => jdo(w, scomb +: kcomb +: ac)
        case '1' => ac match{
          case x +: y +: es => jdo(w, AppExpr(x, y) +: es)
          case x +: es => jdo(w, AppExpr(x, icomb) +: es)
          case _ => jdo(w, AppExpr(icomb, icomb) +: ac)}
        case _ => (collect(ac), src)}
      case _ => (collect(ac), src)}
    val end = inp.lastIndexWhere("01".contains(_))
    if (end == -1) EsoParseFail
    else jdo(inp.take(end + 1), Vector()) match{
      case (res, rem) => EsoParsed(res, rem, rem.size - 1, end + 1)}}
  
  val emptyParser: EsoParser[Seq[Char], Expr] = inp => if(inp.isEmpty) EsoParsed(icomb, Seq(), 0, 0) else EsoParseFail
  
  val lkParser: EsoParser[Seq[Char], Expr] = {
    (unlParser <+> combParser <+> iotaParser <+> jotParser <+> emptyParser)
      .withConditioning(p => filterContains(p.toVector, "*`ski(SKI)01"))
      .withErrors}
  
  def parse(progRaw: String): Try[Expr] = {
    def uncomment(str: String): String = str.replaceAll("""(?m)^#.*$""", "")
    lkParser(uncomment(progRaw)).toTry("Invalid Expression")}
}
