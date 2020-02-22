package lazyk

import common.EsoObj
import lazyk.LazyKFuncs._
import parsers._

import scala.util.Try

object LazyKParsers extends EsoObj{
  val unlParser: EsoParser[Seq[Char], Expr] = {
    PartialRightScanningParser[Char, Expr](_.headOption){
      case ('s', ac) => scomb +: ac
      case ('k', ac) => kcomb +: ac
      case ('i', ac) => icomb +: ac
      case ('`', x +: y +: ac) => AppExpr(x, y) +: ac}}
  
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
    PartialRightScanningParser[Char, Expr](_.headOption){
      case ('i', ac) => iotaexp +: ac
      case ('*', x +: y +: ac) => AppExpr(x, y) +: ac}}
  
  val jotParser: EsoParser[Seq[Char], Expr] = {
    PartialRightScanningParser[Char, Expr](exps => Some(exps.foldLeft(icomb: Expr)(AppExpr))){
      case ('0', ac) => scomb +: kcomb +: ac
      case ('1', x +: y +: ac) => AppExpr(x, y) +: ac
      case ('1', x +: ac) => AppExpr(x, icomb) +: ac
      case ('1', ac) => AppExpr(icomb, icomb) +: ac}}
  
  val emptyParser: EsoParser[Seq[Char], Expr] = inp => if(inp.isEmpty) EsoParsed(icomb, Seq(), 0, 0) else EsoParseFail
  
  val lkParser: EsoParser[Seq[Char], Expr] = {
    (unlParser <+> combParser <+> iotaParser <+> jotParser <+> emptyParser)
      .withConditioning(p => filterContains(p.toVector, "*`ski(SKI)01"))
      .withErrors}
  
  def parse(progRaw: String): Try[Expr] = {
    def uncomment(str: String): String = str.replaceAll("""(?m)^#.*$""", "")
    lkParser(uncomment(progRaw)).toTry("Invalid Expression")}
}
