package parsers

import common.EsoExcep

import scala.util.control.TailCalls.{TailRec, done, tailcall}
import scala.util.{Failure, Success, Try}

trait EsoParseRes[+A] {
  def get: Option[A]
  def passed: Boolean = get.nonEmpty
  def toTry(str: String = "Parse Failed"): Try[A] = get match {
    case Some(res) => Success(res)
    case None => Failure(EsoExcep(str))}
  def orElse[B >: A](alt: => EsoParseRes[B]): EsoParseRes[B]
  def map[B](f: A => B): EsoParseRes[B]
  def flatMap[B](f: A => EsoParseRes[B]): EsoParseRes[B]
  def flatMapWithNext[B](f: A => String => EsoParseRes[B]): EsoParseRes[B]
  def start: Int
  def end: Int
  def length: Int
  def toTramp(start_ind: Int): EsoParseResTramp[A]
}

object EsoParseFail extends EsoParseRes[Nothing]{
  def orElse[B >: Nothing](alt: => EsoParseRes[B]): EsoParseRes[B] = alt
  def map[B](f: Nothing => B): EsoParseRes[B] = EsoParseFail
  def flatMap[B](f: Nothing => EsoParseRes[B]): EsoParseRes[B] = EsoParseFail
  def flatMapWithNext[B](f: Nothing => String => EsoParseRes[B]): EsoParseRes[B] = EsoParseFail
  def get: Option[Nothing] = None
  def length: Int = -1
  def start: Int = -1
  def end: Int = -1
  def toTramp(start_ind: Int): EsoParseResTramp[Nothing] = EsoParseFailTramp
}
case class EsoParsed[+A](parsed: A, rem: String, start: Int, end: Int) extends EsoParseRes[A]{
  def orElse[B >: A](alt: => EsoParseRes[B]): EsoParseRes[B] = this
  def map[B](f: A => B): EsoParseRes[B] = EsoParsed(f(parsed), rem, start, end)
  def flatMap[B](f: A => EsoParseRes[B]): EsoParseRes[B] = f(parsed)
  def flatMapWithNext[B](f: A => String => EsoParseRes[B]): EsoParseRes[B] = f(parsed)(rem)
  def get: Option[A] = Some(parsed)
  def length: Int = end - start
  def toTramp(start_ind: Int): EsoParseResTramp[A] = EsoParsedTramp(parsed, start_ind + start, start_ind + end)
}

trait EsoParseResTramp[+A]{
  def get: Option[A]
  def orElse[B >: A](alt: => EsoParseResTramp[B]): EsoParseResTramp[B]
  def passed: Boolean = get.nonEmpty
  def toTry(str: String = "Parse Failed"): Try[A] = get match {
    case Some(res) => Success(res)
    case None => Failure(EsoExcep(str))}
  def map[B](f: A => B): EsoParseResTramp[B]
  def flatMap[B](f: A => EsoParseResTramp[B]): EsoParseResTramp[B]
  def flatMap[B](f: A => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]]
  def flatMapAll[B](f: (A, Int, Int) => EsoParseResTramp[B]): EsoParseResTramp[B]
  def flatMapAll[B](f: (A, Int, Int) => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]]
  def start: Int
  def end: Int
  def length: Int
  def toFullRes(inp: String): EsoParseRes[A]
}

case class EsoParsedTramp[+A](parsed: A, start: Int, end: Int) extends EsoParseResTramp[A]{
  def get: Option[A] = Some(parsed)
  def orElse[B >: A](alt: => EsoParseResTramp[B]): EsoParseResTramp[B] = this
  def map[B](f: A => B): EsoParseResTramp[B] = EsoParsedTramp(f(parsed), start, end)
  def flatMap[B](f: A => EsoParseResTramp[B]): EsoParseResTramp[B] = f(parsed)
  def flatMap[B](f: A => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = tailcall(f(parsed))
  def flatMapAll[B](f: (A, Int, Int) => EsoParseResTramp[B]): EsoParseResTramp[B] = f(parsed, start, end)
  def flatMapAll[B](f: (A, Int, Int) => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = tailcall(f(parsed, start, end))
  def length: Int = end - start
  def toFullRes(inp: String): EsoParseRes[A] = EsoParsed(parsed, inp.drop(end), start, end)
}

object EsoParseFailTramp extends EsoParseResTramp[Nothing]{
  def get: Option[Nothing] = None
  def orElse[B >: Nothing](alt: => EsoParseResTramp[B]): EsoParseResTramp[B] = alt
  def map[B](f: Nothing => B): EsoParseResTramp[B] = EsoParseFailTramp
  def flatMap[B](f: Nothing => EsoParseResTramp[B]): EsoParseResTramp[B] = EsoParseFailTramp
  def flatMap[B](f: Nothing => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = done(EsoParseFailTramp)
  def flatMapAll[B](f: (Nothing, Int, Int) => EsoParseResTramp[B]): EsoParseResTramp[B] = EsoParseFailTramp
  def flatMapAll[B](f: (Nothing, Int, Int) => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = done(EsoParseFailTramp)
  def length: Int = -1
  def start: Int = -1
  def end: Int = -1
  def toFullRes(inp: String): EsoParseRes[Nothing] = EsoParseFail
}