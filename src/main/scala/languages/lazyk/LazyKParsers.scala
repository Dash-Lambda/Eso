package languages.lazyk

import common.EsoObj
import languages.lazyk.LazyKFuncs._
import parsers._
import parsers.EsoParser._
import parsers.CombinatorFuncs._

import scala.util.Try

object LazyKParsers extends EsoObj{
  val sexpr: Expr = FuncExpr(scomb)
  val kexpr: Expr = FuncExpr(kcomb)
  val iexpr: Expr = FuncExpr(icomb)
  val iotaexpr: Expr = FuncExpr(iotacomb)
  def appExpr(p: (Expr, Expr)): Expr = AppExpr(p._1, p._2)
  
  val jotExprParse: EsoParser[Expr] = {
    (inp: String, ind: Int) =>
      val res = (ind until inp.length)
        .foldRight(Vector[Expr]()){
          case (i, es) => (inp(i), es) match{
            case ('0', ac) => sexpr +: kexpr +: ac
            case ('1', x +: y +: ac) => AppExpr(x, y) +: ac
            case ('1', x +: ac) => AppExpr(x, iexpr) +: ac
            case ('1', ac) => AppExpr(iexpr, iexpr) +: ac
            case (_, ac) => ac}}
      if(res.nonEmpty) Parsed((res.reduceLeft(AppExpr), "", ind, inp.length))
      else ParseFail}
  val jotParse: EsoParser[Expr] = into(R("^[01]+".r), jotExprParse)
  
  val unlExpParse: EsoParser[Expr] = (S("s") ^^^ sexpr) | (S("k") ^^^ kexpr) | (S("i") ^^^ iexpr) | ((S("`") &> (unlExpParse <&> unlExpParse)) map appExpr)
  
  lazy val combParse: EsoParser[Expr] = (S("S") ^^^ sexpr) | (S("K") ^^^ kexpr) | (S("I") ^^^ iexpr) | (S("(") &> ccParse <& S(")"))
  lazy val ccParse: EsoParser[Expr] = combParse.+ map (_.foldLeft(iexpr: Expr)(AppExpr))
  
  val iotaParse: EsoParser[Expr] = (S("i") ^^^ iotaexpr) | ((S("*") &> (iotaParse <&> iotaParse)) map appExpr)
  
  val emptyParser: EsoParser[Expr] = R("""^\Z""".r) ^^^ iexpr
  
  val lkParser: EsoParser[Expr] = unlExpParse | ccParse | iotaParse | jotParse | emptyParser
  
  def parse(progRaw: String): Try[Expr] = {
    def uncommented = progRaw.replaceAll("""(?m)^#.*$""", "")
    def scrubbed =  filterChars(uncommented, "`*skiSKI01()")
    lkParser(scrubbed).toTry("Invalid Expression") map (_._1)}
}
