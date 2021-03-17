package parsers

import common.EsoExcep

import scala.util.{Failure, Success, Try}

trait EsoParseRes[+A] {
  def map[B](f: A => B): EsoParseRes[B]
  
  def flatMap[B](f: A => EsoParseRes[B]): EsoParseRes[B]
  
  def flatMapWithNext[B](f: A => String => EsoParseRes[B]): EsoParseRes[B]
  
  def passed: Boolean
  
  def get: Option[A]
  
  def length: Int
  
  def toTry(str: String = "Parse Failed"): Try[A] = get match {
    case Some(res) => Success(res)
    case None => Failure(EsoExcep(str))
  }
  
  def start: Int
  
  def end: Int
}

object EsoParseFail extends EsoParseRes[Nothing]{
  def map[B](f: Nothing => B): EsoParseRes[B] = EsoParseFail
  def flatMap[B](f: Nothing => EsoParseRes[B]): EsoParseRes[B] = EsoParseFail
  def flatMapWithNext[B](f: Nothing => String => EsoParseRes[B]): EsoParseRes[B] = EsoParseFail
  def passed: Boolean = false
  def get: Option[Nothing] = None
  def length: Int = -1
  def start: Int = -1
  def end: Int = -1
}
case class EsoParsed[+A](parsed: A, rem: String, start: Int, end: Int) extends EsoParseRes[A]{
  def map[B](f: A => B): EsoParseRes[B] = EsoParsed(f(parsed), rem, start, end)
  def flatMap[B](f: A => EsoParseRes[B]): EsoParseRes[B] = f(parsed)
  def flatMapWithNext[B](f: A => String => EsoParseRes[B]): EsoParseRes[B] = f(parsed)(rem)
  def passed: Boolean = true
  def get: Option[A] = Some(parsed)
  def length: Int = end - start
}