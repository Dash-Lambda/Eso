package common

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

trait EsoParseRes[+A, +B]{
  def passed: Boolean
  def start: Int
  def end: Int
  def map[C](f: B => C): EsoParseRes[A, C]
  def flatMap[C, D](f: B => EsoParseRes[C, D]): EsoParseRes[C, D]
}
object EsoParseFail extends EsoParseRes[Nothing, Nothing]{
  def passed: Boolean = false
  def start: Int = -1
  def end: Int = -1
  def map[C](f: Nothing => C): EsoParseRes[Nothing, Nothing] = EsoParseFail
  def flatMap[C, D](f: Nothing => EsoParseRes[C, D]): EsoParseRes[Nothing, Nothing] = EsoParseFail
}
case class EsoParsed[+A, +B](res: B, rem: A, start: Int, end: Int) extends EsoParseRes[A, B]{
  def passed: Boolean = true
  def map[C](f: B => C): EsoParseRes[A, C] = EsoParsed(f(res), rem, start, end)
  def flatMap[C, D](f: B => EsoParseRes[C, D]): EsoParseRes[C, D] = f(res)
}

abstract class OrderedParser[A, B] extends EsoObj{
  def apply(inp: A): EsoParseRes[A, B]
  
  def parseAllValues(inp: A): Vector[B] = parseValuesIterator(inp).toVector
  def parseAllValuesLazy(inp: A): LazyList[B] = parseValuesIterator(inp).to(LazyList)
  def parseValuesIterator(inp: A): Iterator[B] = parseIterator(inp) map (_.res)
  /*def parseValuesIterator(inp: A): Iterator[B] = Iterator.unfold(inp){src =>
    apply(src) match{
      case EsoParsed(res, rem, _) => Some((res, rem))
      case EsoParseFail => None}}*/
  
  def parseAll(inp: A): Vector[EsoParsed[A, B]] = parseIterator(inp).toVector
  def parseAllLazy(inp: A): LazyList[EsoParsed[A, B]] = parseIterator(inp).to(LazyList)
  def parseIterator(inp: A): Iterator[EsoParsed[A, B]] = Iterator.unfold((inp, 0)){
    case (src, tot) =>
      apply(src) match{
        case EsoParsed(res, rem, start, end) => Some((EsoParsed(res, rem, tot + start, tot + end), (rem, tot + end)))
        case EsoParseFail => None}}
  
  def matches(inp: A): Boolean = apply(inp).passed
  
  def map[C](f: B => C): OrderedParser[A, C] = MappedOrderedParser(this, f)
  
  def <+>(p: OrderedParser[A, B]): OrderedMultiParser[A, B] = p match{
    case OrderedMultiParser(ps) => OrderedMultiParser(this +: ps)
    case _ => OrderedMultiParser(Vector(this, p))}
}

case class OrderedMultiParser[A, B](parsers: Vector[OrderedParser[A, B]]) extends OrderedParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = {
    parsers
      .map(p => p(inp))
      .sortBy(r => r.start - r.end)
      .sortBy(_.start)
      .find(_.passed)
      .getOrElse(EsoParseFail)}
  
  override def map[C](f: B => C): OrderedParser[A, C] = OrderedMultiParser(parsers.map(_.map(f)))
  
  override def <+>(p: OrderedParser[A, B]): OrderedMultiParser[A, B] = p match{
    case OrderedMultiParser(ps) => OrderedMultiParser(parsers :++ ps)
    case _ => OrderedMultiParser(parsers :+ p)}
}

case class OrderedRegexParser[B](reg: Regex)(func: Match => B) extends OrderedParser[String, B]{
  def apply(inp: String): EsoParseRes[String, B] = reg.findFirstMatchIn(inp) match{
    case Some(m) => EsoParsed(func(m), m.after.toString, m.start, m.end)
    case None => EsoParseFail}
  
  override def parseIterator(inp: String): Iterator[EsoParsed[String, B]] = {
    reg.findAllMatchIn(inp) map{m =>
      EsoParsed(func(m), m.after.toString, m.start, m.end)}}
  
  override def map[C](f: B => C): OrderedParser[String, C] = OrderedRegexParser(reg)(func andThen f)
  
  override def matches(inp: String): Boolean = reg.matches(inp)
}

object OrderedRegexParser{
  def apply[B](reg: String)(func: Match => B): OrderedRegexParser[B] = OrderedRegexParser(reg.r)(func)
}

case class OrderedPartialParser[A, B](func: PartialFunction[A, (B, A, Int, Int)]) extends OrderedParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = func.lift(inp) match{
    case Some((res, rem, start, end)) => EsoParsed(res, rem, start, end)
    case None => EsoParseFail}
  
  override def map[C](f: B => C): OrderedPartialParser[A, C] = new OrderedPartialParser(func andThen {case (res, rem, start, end) => (f(res), rem, start, end)})
  
  override def matches(inp: A): Boolean = func.isDefinedAt(inp)
}

object OrderedPartialParser{
  def simple[A, B](func: PartialFunction[A, (B, A)]): OrderedPartialParser[A, B] = OrderedPartialParser(func andThen {case (b, a) => (b, a, 0, 0)})
}

case class OrderedChunkParser[A, B](func: A => Option[(B, A, Int, Int)]) extends OrderedParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = func(inp) match{
    case Some((res, rem, start, end)) => EsoParsed(res, rem, start, end)
    case None => EsoParseFail}
  override def map[C](f: B => C): OrderedParser[A, C] = OrderedChunkParser(func andThen (_.map{case (res, rem, start, end) => (f(res), rem, start, end)}))
}

object OrderedChunkParser{
  def simple[A, B](func: A => Option[(B, A)]): OrderedChunkParser[A, B] = OrderedChunkParser(func andThen (_.map{case (b, a) => (b, a, 0, 0)}))
}

case class OrderedRecurParser[A, B](depth: Int, priority: Int = 0)(recur: A => Option[A])(collect: Seq[B] => B)(endParser: OrderedParser[A, B]) extends OrderedParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = {
    trait Cont{
      def apply(x: B): Cont}
    object FinCont extends Cont{
      def apply(x: B): Cont = ResCont(x)}
    case class ResCont(res: B) extends Cont{
      def apply(x: B): Cont = ResCont(res)}
    case class RecCont(d: Int, ac: Vector[B], cc: Cont) extends Cont{
      def apply(x: B): Cont = {
        if(d > 1) RecCont(d - 1, ac :+ x, cc)
        else cc(collect(ac :+ x))}}
    
    @tailrec
    def pdo(src: A, cc: Cont): EsoParseRes[A, B] = {
      recur(src) match{
        case Some(nxt) => pdo(nxt, RecCont(depth, Vector(), cc))
        case None => endParser(src) match{
          case EsoParsed(res, rem, _, _) => pdo(rem, cc(res))
          case EsoParseFail => cc match{
            case ResCont(res) => EsoParsed(res, src, priority, -1)
            case _ => EsoParseFail}}}}
    pdo(inp, FinCont)}
}

case class MappedOrderedParser[A, B, C](parser: OrderedParser[A, B], func: B => C) extends OrderedParser[A, C]{
  def apply(inp: A): EsoParseRes[A, C] = parser(inp) map func
  override def map[D](f: C => D): OrderedParser[A, D] = MappedOrderedParser(parser, func andThen f)
}