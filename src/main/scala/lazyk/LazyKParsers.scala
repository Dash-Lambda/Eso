package lazyk

import common.EsoObj
import lazyk.LazyKFuncs._
import parsers._
import parsers.Implicits._

import scala.util.Try

object LazyKParsers extends EsoObj{
  val sexpr: Expr = FuncExpr(scomb)
  val kexpr: Expr = FuncExpr(kcomb)
  val iexpr: Expr = FuncExpr(icomb)
  val iotaexpr: Expr = FuncExpr(iotacomb)
  def appExpr(p: (Expr, Expr)): Expr = AppExpr(p._1, p._2)
  
  val jotExprParse: EsoParser[Expr] = {
    (inp: String) =>
      val res = inp.toVector
        .filter(c => "01".contains(c))
        .foldRight(Vector[Expr]()){
          case ('0', ac) => sexpr +: kexpr +: ac
          case ('1', x +: y +: ac) => AppExpr(x, y) +: ac
          case ('1', x +: ac) => AppExpr(x, iexpr) +: ac
          case ('1', ac) => AppExpr(iexpr, iexpr) +: ac}
      if(res.nonEmpty) EsoParsed(res.reduceLeft(AppExpr), "", 0, inp.length)
      else EsoParseFail}
  def jotBlockParse: EsoParser[String] = """^[01]+""".r
  def jotParse: EsoParser[Expr] = jotBlockParse >> jotExprParse
  
  def unlExpParse: EsoParser[Expr] = ("s" ^^^ sexpr) | ("k" ^^^ kexpr) | ("i" ^^^ iexpr) | unlParse
  def unlParse: EsoParser[Expr] = ("`" &> (unlExpParse <&> unlExpParse)) map appExpr
  
  def combParse: EsoParser[Expr] = ("S" ^^^ sexpr) | ("K" ^^^ kexpr) | ("I" ^^^ iexpr) | ("(" &> ccParse <& ")")
  def ccParse: EsoParser[Expr] = combParse.+ map (_.foldLeft(iexpr: Expr)(AppExpr))
  
  def iotaExpParse: EsoParser[Expr] = ("i" ^^^ iotaexpr) | iotaParse
  def iotaParse: EsoParser[Expr] = ("*" &> (iotaExpParse <&> iotaExpParse)) map appExpr
  
  val emptyParser: EsoParser[Expr] = """^\Z""".r ^^^ iexpr
  val junkParser: EsoParser[String] = """^[^`*skiSKI01()]*""".r
  
  val lkParser: EsoParser[Expr] = {
    junkParser &> (unlParse | ccParse | iotaParse | jotParse | emptyParser)}
  
  def parse(progRaw: String): Try[Expr] = {
    def uncomment(str: String): String = str.replaceAll("""(?m)^#.*$""", "")
    def scrub(str: String): String = filterChars(str, "`*skiSKI01()")
    lkParser(scrub(uncomment(progRaw))).toTry("Invalid Expression")}
}
