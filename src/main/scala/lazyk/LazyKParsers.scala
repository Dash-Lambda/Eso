package lazyk

import common.EsoObj
import lazyk.LazyKFuncs._
import parsers._

import scala.util.Try

object LazyKParsers extends EsoObj{
  val sexpr: Expr = FuncExpr(scomb)
  val kexpr: Expr = FuncExpr(kcomb)
  val iexpr: Expr = FuncExpr(icomb)
  val iotaexpr: Expr = FuncExpr(iotacomb)
  
  val unlParser: EsoParser[Seq[Char], Expr] = {
    PartialRightScanningParser[Char, Expr](_.headOption){
      case ('s', ac) => sexpr +: ac
      case ('k', ac) => kexpr +: ac
      case ('i', ac) => iexpr +: ac
      case ('`', x +: y +: ac) => AppExpr(x, y) +: ac}}
  
  val combParser: EsoParser[Seq[Char], Expr] = {
    def collect(src: Seq[Expr]): Expr = src.reduceLeft(AppExpr)
    def recur(src: Seq[Char]): ARPRet[Seq[Char], Expr] = src match{
      case c +: cs => c match{
        case '(' => ARPDown(cs, 0, 1)
        case ')' => ARPUp(cs, 0, 1)
        case 'S' => ARPNext(sexpr, cs, 0, 1)
        case 'K' => ARPNext(kexpr, cs, 0, 1)
        case 'I' => ARPNext(iexpr, cs, 0, 1)
        case _ => ARPFail}
      case _ => ARPUp(src, 0, 0)}
    ArbitraryRecurParser(recur _)(collect)}
  
  val iotaParser: EsoParser[Seq[Char], Expr] = {
    PartialRightScanningParser[Char, Expr](_.headOption){
      case ('i', ac) => iotaexpr +: ac
      case ('*', x +: y +: ac) => AppExpr(x, y) +: ac}}
  
  val jotParser: EsoParser[Seq[Char], Expr] = {
    PartialRightScanningParser[Char, Expr](exps => Some(exps.foldLeft(iexpr: Expr)(AppExpr))){
      case ('0', ac) => sexpr +: kexpr +: ac
      case ('1', x +: y +: ac) => AppExpr(x, y) +: ac
      case ('1', x +: ac) => AppExpr(x, iexpr) +: ac
      case ('1', ac) => AppExpr(iexpr, iexpr) +: ac}}
  
  val emptyParser: EsoParser[Seq[Char], Expr] = inp => if(inp.isEmpty) EsoParsed(iexpr, Seq(), 0, 0) else EsoParseFail
  
  val lkParser: EsoParser[Seq[Char], Expr] = {
    (unlParser <+> combParser <+> iotaParser <+> jotParser <+> emptyParser)
      .withConditioning(p => filterContains(p.toVector, "*`ski(SKI)01"))
      .withErrors}
  
  def parse(progRaw: String): Try[Expr] = {
    def uncomment(str: String): String = str.replaceAll("""(?m)^#.*$""", "")
    lkParser(uncomment(progRaw)).toTry("Invalid Expression")}
}
