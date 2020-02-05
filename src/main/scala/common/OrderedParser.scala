package common

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

trait EsoParseRes[+A, +B]{
  def passed: Boolean
  def ind: Int}
object EsoParseFail extends EsoParseRes[Nothing, Nothing]{
  def passed: Boolean = false
  def ind: Int = -1}
case class EsoParsed[+A, +B](res: B, rem: A, ind: Int) extends EsoParseRes[A, B]{
  def passed: Boolean = true}

abstract class OrderedParser[A, B] extends EsoObj{
  def apply(inp: A): EsoParseRes[A, B]
  
  def parseAll(inp: A): Vector[B]
  def parseAllLazy(inp: A): LazyList[B]
  def parseIterator(inp: A): Iterator[B]
  
  def <+>(p: OrderedParser[A, B]): OrderedMultiParser[A, B] = p match{
    case OrderedMultiParser(ps) => OrderedMultiParser(this +: ps)
    case _ => OrderedMultiParser(Vector(this, p))}
}

case class OrderedMultiParser[A, B](parsers: Vector[OrderedParser[A, B]]) extends OrderedParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = {
    parsers
      .map(p => p(inp))
      .sortBy(_.ind)
      .find(_.passed)
      .getOrElse(EsoParseFail)}
  
  def parseAll(inp: A): Vector[B] = parseIterator(inp).toVector
  def parseAllLazy(inp: A): LazyList[B] = parseIterator(inp).to(LazyList)
  def parseIterator(inp: A): Iterator[B] = Iterator.unfold(inp){src =>
    apply(src) match{
      case EsoParsed(res, rem, _) => Some((res, rem))
      case EsoParseFail => None}}
  
  override def <+>(p: OrderedParser[A, B]): OrderedMultiParser[A, B] = p match{
    case OrderedMultiParser(ps) => OrderedMultiParser(parsers :++ ps)
    case _ => OrderedMultiParser(parsers :+ p)}
}

case class OrderedRegexParser[B](reg: Regex)(f: Match => B) extends OrderedParser[String, B]{
  def apply(inp: String): EsoParseRes[String, B] = reg.findFirstMatchIn(inp) match{
    case Some(m) => EsoParsed(f(m), m.after.toString, m.start)
    case None => EsoParseFail
  }
  
  def parseAll(inp: String): Vector[B] = ???
  
  def parseAllLazy(inp: String): LazyList[B] = ???
  
  def parseIterator(inp: String): Iterator[B] = ???
}