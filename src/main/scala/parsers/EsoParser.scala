package parsers

import common.{EsoExcep, EsoObj}

import scala.util.control.TailCalls._
import scala.util.matching.Regex

abstract class EsoParser[+A] extends (String => EsoParseRes[A]) with EsoObj{
  def apply(inp: String): EsoParseRes[A]
  def parseOne(inp: String): A = apply(inp) match{
    case EsoParsed(res, _, _, _) => res
    case _ => throw EsoExcep("Parse Failure")}
  def matches(inp: String): Boolean = apply(inp).passed
  
  /* p ^^ f = p map f */
  def ^^[B](f: A => B): EsoParser[B] = map(f)
  def map[B](f: A => B): EsoParser[B] = EsoMapParser(this, f)
  /* p ^^^ v = if p then v */
  def ^^^[B](v: => B): EsoParser[B] = EsoMapParser(this, (_: A) => v)
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
  
  def parseAllInPlaceIterator(inp: String): Iterator[EsoParsed[A]] = Iterator.unfold((inp, 0)){
    case (src, stp) => apply(src) match{
      case EsoParsed(p, r, s, e) => Some((EsoParsed(p, r, stp + s, stp + e), (r, stp + e)))
      case _ => None}}
  
  /* Parse all matches, no fail on empty */
  def * : EsoParser[Vector[A]] = all
  def all: EsoParser[Vector[A]] = {
    inp => {
      val parses = parseAllInPlaceIterator(inp).toVector
      if (parses.nonEmpty) EsoParsed(parses map (_.parsed), parses.last.rem, parses.head.start, parses.last.end)
      else EsoParsed(Vector(), inp, 0, 0)}}
  /* Parse all matches, fail on empty */
  def + : EsoParser[Vector[A]] = atLeastOne
  def atLeastOne: EsoParser[Vector[A]] = {
    inp => {
      val parses = parseAllInPlaceIterator(inp).toVector
      if (parses.nonEmpty) EsoParsed(parses map (_.parsed), parses.last.rem, parses.head.start, parses.last.end)
      else EsoParseFail}}
  
  /* Alternative composition, first match */
  def |[B >: A](q: => EsoParser[B]): EsoParser[B] = q match{
    case EsoAltParser(qs) => EsoAltParser(this +: qs)
    case _ => EsoAltParser(Vector(this, q))}
  /* Alternative composition, Earliest match */
  def ||[B >: A](q: => EsoParser[B]): EsoParser[B] = q match{
    case EsoEarliestMatchParser(qs) => EsoEarliestMatchParser(this +: qs)
    case _ => EsoEarliestMatchParser(Vector(this, q))}
  /* Alternative composition, longest match */
  def |||[B >: A](q: => EsoParser[B]): EsoParser[B] = q match{
    case EsoLongestMatchParser(qs) => EsoLongestMatchParser(this +: qs)
    case _ => EsoLongestMatchParser(Vector(this, q))}
  
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
  
  def tramp[B](inp: String)(cc: EsoParseRes[A] => TailRec[EsoParseRes[B]]): TailRec[EsoParseRes[B]] = cc(apply(inp))
}

object Implicits{
  implicit def string2parser(str: String): EsoParser[String] = EsoStringParser(str)
  implicit def regex2parser(reg: Regex): EsoParser[String] = EsoRegexParser(reg)
}