package common

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

trait EsoParseRes[+A, +B]{
  def passed: Boolean
  def ind: Int
  def map[C](f: B => C): EsoParseRes[A, C]
  def flatMap[C, D](f: B => EsoParseRes[C, D]): EsoParseRes[C, D]
}
object EsoParseFail extends EsoParseRes[Nothing, Nothing]{
  def passed: Boolean = false
  def ind: Int = -1
  def map[C](f: Nothing => C): EsoParseRes[Nothing, Nothing] = EsoParseFail
  def flatMap[C, D](f: Nothing => EsoParseRes[C, D]): EsoParseRes[Nothing, Nothing] = EsoParseFail
}
case class EsoParsed[+A, +B](res: B, rem: A, ind: Int) extends EsoParseRes[A, B]{
  def passed: Boolean = true
  def map[C](f: B => C): EsoParseRes[A, C] = EsoParsed(f(res), rem, ind)
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
        case EsoParsed(res, rem, ind) => Some((EsoParsed(res, rem, tot + ind), (rem, tot + ind)))
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
      .sortBy(_.ind)
      .find(_.passed)
      .getOrElse(EsoParseFail)}
  
  override def map[C](f: B => C): OrderedParser[A, C] = OrderedMultiParser(parsers.map(_.map(f)))
  
  override def <+>(p: OrderedParser[A, B]): OrderedMultiParser[A, B] = p match{
    case OrderedMultiParser(ps) => OrderedMultiParser(parsers :++ ps)
    case _ => OrderedMultiParser(parsers :+ p)}
}

case class OrderedRegexParser[B](reg: Regex)(func: Match => B) extends OrderedParser[String, B]{
  def apply(inp: String): EsoParseRes[String, B] = reg.findFirstMatchIn(inp) match{
    case Some(m) => EsoParsed(func(m), m.after.toString, m.start)
    case None => EsoParseFail}
  
  override def parseIterator(inp: String): Iterator[EsoParsed[String, B]] = {
    reg.findAllMatchIn(inp) map{m =>
      EsoParsed(func(m), m.after.toString, m.start)}}
  
  override def map[C](f: B => C): OrderedParser[String, C] = OrderedRegexParser(reg)(func andThen f)
  
  override def matches(inp: String): Boolean = reg.matches(inp)
}

case class OrderedPartialParser[A, B](func: PartialFunction[A, (B, A, Int)]) extends OrderedParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = func.lift(inp) match{
    case Some((res, rem, ind)) => EsoParsed(res, rem, ind)
    case None => EsoParseFail}
  
  override def map[C](f: B => C): OrderedPartialParser[A, C] = OrderedPartialParser(func andThen {case (res, rem, ind) => (f(res), rem, ind)})
  
  override def matches(inp: A): Boolean = func.isDefinedAt(inp)
}

case class OrderedChunkParser[A, B](func: A => Option[(B, A, Int)]) extends OrderedParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = func(inp) match{
    case Some((res, rem, ind)) => EsoParsed(res, rem, ind)
    case None => EsoParseFail}
  override def map[C](f: B => C): OrderedParser[A, C] = OrderedChunkParser(func andThen (_.map{case (res, rem, ind) => (f(res), rem, ind)}))
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
          case EsoParsed(res, rem, _) => pdo(rem, cc(res))
          case EsoParseFail => cc match{
            case ResCont(res) => EsoParsed(res, src, priority)
            case _ => EsoParseFail}}}}
    pdo(inp, FinCont)}
}

case class MappedOrderedParser[A, B, C](parser: OrderedParser[A, B], func: B => C) extends OrderedParser[A, C]{
  def apply(inp: A): EsoParseRes[A, C] = parser(inp) map func
  override def map[D](f: C => D): OrderedParser[A, D] = MappedOrderedParser(parser, func andThen f)
}