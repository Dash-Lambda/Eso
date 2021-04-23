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
  def toTramp(inp: EsoParserInput, start_ind: Int): EsoParseResTramp[A]
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
  def toTramp(inp: EsoParserInput, start_ind: Int): EsoParseResTramp[Nothing] = EsoParseFailTramp(inp)
}
case class EsoParsed[+A](parsed: A, rem: String, start: Int, end: Int) extends EsoParseRes[A]{
  def orElse[B >: A](alt: => EsoParseRes[B]): EsoParseRes[B] = this
  def map[B](f: A => B): EsoParseRes[B] = EsoParsed(f(parsed), rem, start, end)
  def flatMap[B](f: A => EsoParseRes[B]): EsoParseRes[B] = f(parsed)
  def flatMapWithNext[B](f: A => String => EsoParseRes[B]): EsoParseRes[B] = f(parsed)(rem)
  def get: Option[A] = Some(parsed)
  def length: Int = end - start
  def toTramp(inp: EsoParserInput, start_ind: Int): EsoParseResTramp[A] = EsoParsedTramp(parsed, inp, start_ind + start, start_ind + end)
}

trait EsoParseResTramp[+A]{
  def inp: EsoParserInput
  def start: Int
  def end: Int
  def length: Int
  def get: Option[A]
  def toFullRes(inp: String): EsoParseRes[A]
  def withInp(ninp: EsoParserInput): EsoParseResTramp[A]
  def map[B](f: (A, EsoParserInput, Int, Int) => TailRec[(B, EsoParserInput, Int, Int)]): TailRec[EsoParseResTramp[B]]
  def flatMap[B](f: (A, EsoParserInput, Int, Int) => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]]
  def orElse[B >: A](alt: => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]]
  
  def passed: Boolean = get.nonEmpty
  def toTry(str: String = "Parse Failed"): Try[A] = get match {
    case Some(res) => Success(res)
    case None => Failure(EsoExcep(str))}
  def isBefore[B](q: EsoParseResTramp[B]): Boolean = passed && (start <= q.start || !q.passed)
  def isLongerThan[B](q: EsoParseResTramp[B]): Boolean = passed && (length >= q.length || !q.passed)
  def map[B](f: (A, EsoParserInput, Int, Int) => (B, EsoParserInput, Int, Int)): EsoParseResTramp[B] = map{case (p, i, s, e) => done(f(p, i, s, e))}.result
  def flatMap[B](f: (A, EsoParserInput, Int, Int) => EsoParseResTramp[B]): EsoParseResTramp[B] = flatMap{case (p, i, s, e) => done(f(p, i, s, e))}.result
  def orElse[B >: A](alt: => EsoParseResTramp[B]): EsoParseResTramp[B] = orElse(done(alt)).result
}

case class EsoParsedTramp[+A](parsed: A, inp: EsoParserInput,  start: Int, end: Int) extends EsoParseResTramp[A]{
  def length: Int = end - start
  def get: Option[A] = Some(parsed)
  def toFullRes(inp: String): EsoParseRes[A] = EsoParsed(parsed, inp.drop(end), start, end)
  def withInp(ninp: EsoParserInput): EsoParseResTramp[A] = EsoParsedTramp(parsed, ninp, start, end)
  
  def map[B](f: (A, EsoParserInput, Int, Int) => TailRec[(B, EsoParserInput, Int, Int)]): TailRec[EsoParseResTramp[B]] = tailcall(f(parsed, inp, start, end) map {case (p, i, s, e) => EsoParsedTramp(p, i, s, e)})
  def flatMap[B](f: (A, EsoParserInput, Int, Int) => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = tailcall(f(parsed, inp, start, end))
  def orElse[B >: A](alt: => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = done(this)
}

case class EsoParseFailTramp(inp: EsoParserInput) extends EsoParseResTramp[Nothing]{
  def length: Int = -1
  def start: Int = -1
  def end: Int = -1
  def get: Option[Nothing] = None
  def toFullRes(inp: String): EsoParseRes[Nothing] = EsoParseFail
  def withInp(ninp: EsoParserInput): EsoParseResTramp[Nothing] = EsoParseFailTramp(ninp)
  
  def map[B](f: (Nothing, EsoParserInput, Int, Int) => TailRec[(B, EsoParserInput, Int, Int)]): TailRec[EsoParseResTramp[B]] = done(this)
  def flatMap[B](f: (Nothing, EsoParserInput, Int, Int) => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = done(this)
  def orElse[B >: Nothing](alt: => TailRec[EsoParseResTramp[B]]): TailRec[EsoParseResTramp[B]] = tailcall(alt)
}