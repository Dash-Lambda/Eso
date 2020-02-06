package ui

import common.{EsoExcep, EsoObj}

import scala.io.Source
import scala.util.{Failure, Success, Try}

object EsoFileReader extends EsoObj{
  val encodings: LazyList[String] = LazyList("UTF-8", "Cp1252", "UTF-16")
  
  def getSource(fnam: String, normLines: Boolean = true): Option[Try[String]] = encodings
    .map(e => readFileWithEncoding(fnam, e, normLines))
    .collectFirst {
      case s: Success[String] => s
      case Failure(ex: java.io.FileNotFoundException) => Failure(ex)}
  
  def readFile(fnam: String, normLines: Boolean = true): Try[String] = {
    getSource(fnam, normLines)
      .getOrElse(Failure(EsoExcep("Incompatible File Encoding")))}
  def readFileWithEncoding(fnam: String, enc: String, normLines: Boolean = true): Try[String] = Try{
    val src = Source.fromFile(fnam, enc)
    val res = if(normLines) src.mkString.replaceAllLiterally("\r\n", "\n") else src.mkString
    src.close()
    res}
}
