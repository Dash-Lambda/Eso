package ui

import java.io.{File, PrintWriter}

import translators._

import scala.annotation.tailrec
import scala.io.{Source, StdIn}
import scala.util.Try

case class HandlerException(info: String) extends Throwable

object ConsoleHandlers {
  def translate(trans: BFTranslator, dir: Boolean)(sourceNam: String): String = if(dir) trans(grabProg(sourceNam)) else trans.unapply(grabProg(sourceNam))
  def translateHandler(trans: BFTranslator, dir: String)(fnams: Vector[String]): Try[String] = Try{
    if(dir == "from" || dir == "to"){
      fnams match{
        case f1 +: f2 +: _ =>
          val prog = translate(trans, dir == "from")(f1)
          val oFile = new PrintWriter(new File(f2))
          oFile.print(prog)
          oFile.close()
          prog
        case f +: _ => translate(trans, dir == "from")(f)
        case _ => throw HandlerException("No File Arguments")
      }
    }else{
      throw HandlerException("Invalid Direction")
    }
  }
  
  def runProgHandler(interp: String => Option[String], log: Boolean)(fnam: String): Try[String] = Try{
    val prog = grabProg(fnam)
    if(log) println(s"Running $fnam...")
    interp(prog) match{
      case Some(str) => str
      case None => throw HandlerException("Program Execution Error")
    }
  }
  
  def grabBFLangs(fnam: String): Try[Vector[BFTranslator]] = {
    @tailrec
    def lHelper(ac: Vector[BFTranslator], src: Vector[String]): Vector[BFTranslator] = src match{
      case t +: ts if t.startsWith("name") =>
        val nam = t.dropWhile(_ != '=').tail
        val kvs = ts.take(8).map{str => str.splitAt(str.indexWhere(_ == '='))}.map(p => (p._1, p._2.tail))
        lHelper(ac :+ GenericBFTranslator(nam, kvs), ts.dropWhile(str => !str.startsWith("name")))
      case _ +: ts => lHelper(ac, ts.dropWhile(str => !str.startsWith("name")))
      case _ => ac
    }
    
    Try{
      val iFile = Source.fromFile(fnam)
      val res = lHelper(Vector[BFTranslator](), iFile.getLines.toVector)
      iFile.close()
      res
    }
  }
  
  def grabStr(prompt: String): String = {print(prompt); StdIn.readLine}
  def grabProg(fnam: String): String = {
    val fin = Source.fromFile(fnam)
    val str = fin.mkString
    fin.close()
    str
  }
}
