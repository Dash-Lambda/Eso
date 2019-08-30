package whitespace

import common.{Config, EsoExcep, Translator}
import WSCommon._
import spire.math.SafeLong

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object WSAssembly extends Translator{
  val name: String = "WSAssembly"
  val baseLang: String = "WhiteSpace"
  
  def apply(config: Config)(prog: String): Try[String] = {
    @tailrec
    def ado(ac: String, src: Vector[String]): Try[String] = src match{
      case op +: ops =>
        if(op.startsWith("//")) ado(ac, ops)
        else if(nonArgOps.contains(op)) ado(ac ++ revMap(op), ops)
        else argOps.find(op.startsWith) match{
          case Some(key) =>
            if(op.exists(_.isDigit)){
              val assembled = revMap(key) ++ binNum(SafeLong(BigInt(op.drop(key.length))))
              ado(ac ++ assembled, ops)
            }else Failure(EsoExcep(s"Not Enough Arguments ($op)"))
          case None => Failure(EsoExcep(s"Operation Not Recognized ($op)"))
        }
      case _ =>
        val noted = ac.toVector.flatMap{
          case '\t' => "T\t"
          case '\n' => "L\n"
          case ' ' => "S "}
        Success(noted.mkString)
    }
    
    val cleaned = prog.split("\n")
      .map(_.replaceAll("[\t\r\n ]", ""))
      .filter(_.nonEmpty)
      .toVector
    ado("", cleaned)
  }
  def unapply(config: Config)(prog: String): Try[String] = {
    val res = condition(prog)
      .map{case (tag, num) => s"$tag${if(argOps.contains(tag)) s" $num" else ""}"}
      .mkString("\n")
    Success(res)
  }
}
