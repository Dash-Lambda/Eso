package parsers

import common.EsoObj

import scala.util.control.TailCalls._
import scala.util.matching.Regex

case class EsoParserInput(inp: String) extends CharSequence with IndexedSeq[Char]{ // Stuck this in to make it easier to play with memoization
  def string: String = inp
  def apply(i: Int): Char = charAt(i)
  def charAt(index: Int): Char = inp.charAt(index)
  def subSequence(start: Int, end: Int): CharSequence = inp.subSequence(start, end)
  def length: Int = inp.length
  
  // Explicitly defining these speeds it up a little
  def startsWith(prefix: String): Boolean = inp.startsWith(prefix)
  def startsWith(prefix: String, offset: Int): Boolean = inp.startsWith(prefix, offset)
  
  override def toString: String = inp
}

abstract class EsoParser[+A] extends (String => EsoParseRes[A]) with EsoObj{
  def apply(inp: String): EsoParseRes[A]
  def matches(inp: String): Boolean = apply(inp).passed
  
  /* p ^^ f = p map f */
  def ^^[B](f: A => B): EsoParser[B] = map(f)
  def map[B](f: A => B): EsoParser[B] = EsoMapParser(this)(f)
  /* p ^^^ v = if p then v */
  def ^^^[B](v: => B): EsoParser[B] = EsoMapParser(this)(_ => v)
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
  /* Only pass if cond is true */
  def onlyif(cond: (A, Int, Int) => Boolean): EsoParser[A] = EsoFlatMapResultParser(this){case (pr, pi, ps, pe) => done(if(cond(pr, ps, pe)) EsoParsedTramp(pr, pi, ps, pe) else EsoParseFailTramp(pi))}
  
  /* Alternative composition, first match */
  def |[B >: A](q: => EsoParser[B]): EsoParser[B] = EsoAltParser(this, q)
  /* Alternative composition, Earliest match */
  def ||[B >: A](q: => EsoParser[B]): EsoParser[B] = EsoSortedAltParser(this, q)(_ isBefore _)
  /* Alternative composition, longest match */
  def |||[B >: A](q: => EsoParser[B]): EsoParser[B] = EsoSortedAltParser(this, q)(_ isLongerThan _)
  
  /* p <| q = p if q, ignore input consumed by q*/
  def <|[B](q: => EsoParser[B]): EsoParser[A] = EsoFlatMapResultParser(this){
    case (pr, pi, ps, pe) =>
      tailcall(
        q.tramp(pi, pe){
          qres =>
            qres.flatMap{
              case (_, qi, _, _) =>
                done(EsoParsedTramp(pr, qi, ps, pe))}})}
  /* p <& q = p if q, q consumes input */
  def <&[B](q: => EsoParser[B]): EsoParser[A] = <&>(q) map {case (a, _) => a}
  /* p &> q = if p then q, p consumes input */
  def &>[B](q: => EsoParser[B]): EsoParser[B] = <&>(q) map {case (_, b) => b}
  /* p <&> q = (p, q) */
  def <&>[B](q: => EsoParser[B]): EsoParser[(A, B)] = new EsoProdParser(this, q)
  /* Into */
  def >>[B](q: => EsoParser[B])(implicit ev: EsoParser[A] <:< EsoParser[String]): EsoParser[B] = EsoIntoParser(ev(this), q)
  /* Concat */
  def <+>(q: => EsoParser[String])(implicit ev: EsoParser[A] <:< EsoParser[String]): EsoParser[String] = ev(this).<&>(q) map {case (a, b) => a + b}
  
  type ParseTrampResult[B] = EsoParseResTramp[B]
  type ParserContinuation[B, C] = EsoParseResTramp[B] => TailRec[EsoParseResTramp[C]]
  def applyByTramp(inp: String): EsoParseRes[A] = tramp(EsoParserInput(inp), 0)(done).result.toFullRes(inp)
  def tramp[AA >: A, B](inp: EsoParserInput, start_ind: Int)(cc: ParserContinuation[AA, B]): TailRec[ParseTrampResult[B]] = {
    tailcall(
      cc(apply(inp.string.drop(start_ind)).toTramp(inp, start_ind)))}
}

object Implicits{
  implicit def string2parser(str: String): EsoParser[String] = EsoStringParser(str)
  implicit def regex2parser(reg: Regex): EsoParser[String] = EsoRegexParser(reg)
}