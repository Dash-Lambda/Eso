package ui

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}

import common.{EsoExcep, EsoObj}

import scala.collection.{immutable, mutable}
import scala.io.Source
import scala.util.{Failure, Success, Try}

trait EsoFileInterface extends EsoObj{
  def readFile(fnam: String, normLines: Boolean = true): Try[String]
  def readFileWithEncoding(fnam: String, enc: String, normLines: Boolean = true): Try[String]
  def getLastModified(fnam: String): Try[Long]
  
  def writeFile(fnam: String, str: String): EsoFileInterface
  def appendFile(fnam: String, str: String): EsoFileInterface
  
  def fileExists(fnam: String): Boolean
  
  def getFile(fnam: String, normLines: Boolean = true): Option[String] = readFile(fnam, normLines) match{
    case Success(str) => Some(str)
    case _ => None}
}

object SystemFileInterface extends EsoFileInterface{
  val encodings: LazyList[String] = LazyList("UTF-8", "Cp1252", "UTF-16")
  
  def readFile(fnam: String, normLines: Boolean): Try[String] = {
    val res = encodings
      .map(e => readFileWithEncoding(fnam, e, normLines))
      .collectFirst {
        case s: Success[String] => s
        case Failure(ex: java.io.FileNotFoundException) => Failure(ex)}
    res.getOrElse(Failure(EsoExcep("Incompatible File Encoding")))}
  def readFileWithEncoding(fnam: String, enc: String, normLines: Boolean): Try[String] = Try{
    val src = Source.fromFile(fnam, enc)
    val res = if(normLines) src.mkString.replace("\r\n", "\n") else src.mkString
    src.close()
    res}
  
  def getLastModified(fnam: String): Try[Long] = Try{
    val f = new File(fnam)
    f.lastModified()}
  
  def writeFile(fnam: String, str: String): EsoFileInterface = {
    val of = new PrintWriter(new File(fnam))
    of.print(str)
    of.close()
    this}
  
  def appendFile(fnam: String, str: String): EsoFileInterface = {
    val of = new PrintWriter(new File(fnam))
    of.append(str)
    of.close()
    this}
  
  def fileExists(fnam: String): Boolean = Files.exists(Paths.get(fnam))
}

case class MutableContainedFileInterface(fileMap: mutable.HashMap[String, (String, Long)], timer: () => Long = System.currentTimeMillis) extends EsoFileInterface {
  def readFile(fnam: String, normLines: Boolean): Try[String] = fileMap.get(fnam) match{
    case Some((txt, _)) => Success(txt)
    case None => Failure(EsoExcep("File Not Found"))}
  def readFileWithEncoding(fnam: String, enc: String, normLines: Boolean): Try[String] = readFile(fnam, normLines)
  
  def getLastModified(fnam: String): Try[Long] = fileMap.get(fnam) match{
    case Some((_, lmt)) => Success(lmt)
    case None => Failure(EsoExcep("File Not Found"))}
  
  def writeFile(fnam: String, str: String): EsoFileInterface = {
    fileMap += ((fnam, (str, timer())))
    this}
  
  def appendFile(fnam: String, str: String): EsoFileInterface = {
    fileMap += ((fnam, (fileMap.getOrElse(fnam, ("", 0))._1 ++ str, timer())))
    this}
  
  def fileExists(fnam: String): Boolean = fileMap.isDefinedAt(fnam)
}
object MutableContainedFileInterface{
  def withElms(kvs: (String, (String, Long))*): MutableContainedFileInterface =
    MutableContainedFileInterface(mutable.HashMap.from(kvs), () => System.currentTimeMillis)
}

case class ImmutableContainedFileInterface(fileMap: immutable.HashMap[String, (String, Long)], timer: () => Long = System.currentTimeMillis) extends EsoFileInterface{
  def readFile(fnam: String, normLines: Boolean): Try[String] = fileMap.get(fnam) match{
    case Some((txt, _)) => Success(txt)
    case None => Failure(EsoExcep("File Not Found"))}
  def readFileWithEncoding(fnam: String, enc: String, normLines: Boolean): Try[String] = readFile(fnam, normLines)
  
  def getLastModified(fnam: String): Try[Long] = fileMap.get(fnam) match{
    case Some((_, lmt)) => Success(lmt)
    case None => Failure(EsoExcep("File Not Found"))}
  
  def writeFile(fnam: String, str: String): EsoFileInterface = ImmutableContainedFileInterface(fileMap + ((fnam, (str, timer()))), timer)
  
  def appendFile(fnam: String, str: String): EsoFileInterface = ImmutableContainedFileInterface(fileMap + ((fnam, (fileMap.getOrElse(fnam, ("", 0))._1 ++ str, timer()))), timer)
  
  def fileExists(fnam: String): Boolean = fileMap.isDefinedAt(fnam)
}
object ImmutableContainedFileInterface{
  def withElms(kvs: (String, (String, Long))*): ImmutableContainedFileInterface =
    ImmutableContainedFileInterface(immutable.HashMap.from(kvs), () => System.currentTimeMillis)
}