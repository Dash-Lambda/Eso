package parsers

import common.EsoObj

import scala.util.control.TailCalls._
import scala.util.matching.Regex

case class EsoParserInput(str: String) extends CharSequence with IndexedSeq[Char]{ // Stuck this in to make it easier to play with memoization
  def apply(i: Int): Char = str(i)
  def charAt(index: Int): Char = str(index)
  def subSequence(start: Int, end: Int): CharSequence = str.subSequence(start, end)
  def length: Int = str.length
}

abstract class EsoParser[+A] extends (String => EsoParseRes[A]) with EsoObj{
  def apply(inp: String): EsoParseRes[A]
  def matches(inp: String): Boolean = apply(inp).passed
  
  /* p ^^ f = p map f */
  def ^^[B](f: A => B): EsoParser[B] = map(f)
  def map[B](f: A => B): EsoParser[B] = EsoMapParser(this, f)
  /* p ^^^ v = if p then v */
  def ^^^[B](v: => B): EsoParser[B] = EsoConstantParser(this, v)
  def flatMap[B](f: A => EsoParser[B]): EsoParser[B] = EsoFlatMappedParser(this, f)
  
  def parseAllValues(inp: String): Vector[A] = parseValuesIterator(inp).toVector
  def parseAllValuesLazy(inp: String): LazyList[A] = parseValuesIterator(inp).to(LazyList)
  def parseValuesIterator(inp: String): Iterator[A] = parseIterator(inp) map (_.parsed)
  
  def parseAll(inp: String): Vector[EsoParsed[A]] = parseIterator(inp).toVector
  def parseAllLazy(inp: String): LazyList[EsoParsed[A]] = parseIterator(inp).to(LazyList)
  def parseIterator(inp: String): Iterator[EsoParsed[A]] = Iterator.unfold(inp){
    src => apply(src) match{
      case EsoParsed(tok, rem, start, end) => Some((EsoParsed(tok, rem, start, end), rem))
      case _ => None}}
  
  /* Parse all matches, no fail on empty */
  def * : EsoParser[Vector[A]] = all
  def all: EsoParser[Vector[A]] = EsoAllParser(this, 0)
  /* Parse all matches, fail on empty */
  def + : EsoParser[Vector[A]] = atLeastOne
  def atLeastOne: EsoParser[Vector[A]] = EsoAllParser(this, 1)
  
  /* Alternative composition, first match */
  def |[B >: A](q: => EsoParser[B]): EsoParser[B] = EsoAltParser(this, q)
  /* Alternative composition, Earliest match */
  def ||[B >: A](q: => EsoParser[B]): EsoParser[B] = EsoEarliestMatchParser(this, q)
  /* Alternative composition, longest match */
  def |||[B >: A](q: => EsoParser[B]): EsoParser[B] = EsoLongestMatchParser(this, q)
  
  /* p <| q = p if q, ignore input consumed by q*/
  def <|[B](q: => EsoParser[B]): EsoParser[A] = new EsoLCondParser(this, q)
  /* p <& q = p if q, q consumes input */
  def <&[B](q: => EsoParser[B]): EsoParser[A] = new EsoLImpParser(this, q)
  /* p &> q = if p then q, p consumes input */
  def &>[B](q: => EsoParser[B]): EsoParser[B] = new EsoRImpParser(this, q)
  /* p <&> q = (p, q) */
  def <&>[B](q: => EsoParser[B]): EsoParser[(A, B)] = new EsoProdParser(this, q)
  /* Into */
  def >>[B](q: => EsoParser[B])(implicit ev: EsoParser[A] <:< EsoParser[String]): EsoParser[B] = EsoIntoParser(ev(this), q)
  /* Concat */
  def <+>(q: => EsoParser[String])(implicit ev: EsoParser[A] <:< EsoParser[String]): EsoParser[String] = EsoConcatParser(ev(this), q)
  
  type ParseTrampResult[B] = EsoParseResTramp[B]
  type ParserContinuation[B, C] = EsoParseResTramp[B] => TailRec[EsoParseResTramp[C]]
  def applyByTramp(inp: String): EsoParseRes[A] = tramp(EsoParserInput(inp), 0)(done).result.toFullRes(inp)
  def tramp[AA >: A, B](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, B]): TailRec[ParseTrampResult[B]] = {
    tailcall(
      cc(apply(inp.str.drop(start_ind)).toTramp(start_ind)))}
}

object Implicits{
  implicit def string2parser(str: String): EsoParser[String] = EsoStringParser(str)
  implicit def regex2parser(reg: Regex): EsoParser[String] = EsoRegexParser(reg)
}