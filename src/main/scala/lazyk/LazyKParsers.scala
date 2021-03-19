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
        .foldRight(Vector[Expr]()){
          case ('0', ac) => sexpr +: kexpr +: ac
          case ('1', x +: y +: ac) => AppExpr(x, y) +: ac
          case ('1', x +: ac) => AppExpr(x, iexpr) +: ac
          case ('1', ac) => AppExpr(iexpr, iexpr) +: ac
          case (_, ac) => ac}
      if(res.nonEmpty) EsoParsed(res.reduceLeft(AppExpr), "", 0, inp.length)
      else EsoParseFail}
  val jotParse: EsoParser[Expr] = "^[01]+".r >> jotExprParse
  
  val unlExpParse: EsoParser[Expr] = ("s" ^^^ sexpr) | ("k" ^^^ kexpr) | ("i" ^^^ iexpr) | (("`" &> (unlExpParse <&> unlExpParse)) map appExpr)
  
  lazy val combParse: EsoParser[Expr] = ("S" ^^^ sexpr) | ("K" ^^^ kexpr) | ("I" ^^^ iexpr) | ("(" &> ccParse <& ")")
  val ccParse: EsoParser[Expr] = combParse.+ map (_.foldLeft(iexpr: Expr)(AppExpr))
  
  val iotaParse: EsoParser[Expr] = ("i" ^^^ iotaexpr) | (("*" &> (iotaParse <&> iotaParse)) map appExpr)
  
  val emptyParser: EsoParser[Expr] = """^\Z""".r ^^^ iexpr
  
  val lkParser: EsoParser[Expr] = unlExpParse | ccParse | iotaParse | jotParse | emptyParser
  
  def parse(progRaw: String): Try[Expr] = {
    def uncommented = progRaw.replaceAll("""(?m)^#.*$""", "")
    def scrubbed =  filterChars(uncommented, "`*skiSKI01()")
    lkParser(scrubbed).toTry("Invalid Expression")}
}
