package parsers

import common.EsoObj

trait EsoParseRes[+A, +B]{
  def passed: Boolean
  def start: Int
  def end: Int
  def map[C](f: B => C): EsoParseRes[A, C]
  def flatMap[C, D](f: B => EsoParseRes[C, D]): EsoParseRes[C, D]
  def toOption: Option[B]
}
object EsoParseFail extends EsoParseRes[Nothing, Nothing]{
  def passed: Boolean = false
  def start: Int = -1
  def end: Int = -1
  def map[C](f: Nothing => C): EsoParseRes[Nothing, Nothing] = EsoParseFail
  def flatMap[C, D](f: Nothing => EsoParseRes[C, D]): EsoParseRes[Nothing, Nothing] = EsoParseFail
  def toOption: Option[Nothing] = None
}
case class EsoParsed[+A, +B](res: B, rem: A, start: Int, end: Int) extends EsoParseRes[A, B]{
  def passed: Boolean = true
  def map[C](f: B => C): EsoParseRes[A, C] = EsoParsed(f(res), rem, start, end)
  def flatMap[C, D](f: B => EsoParseRes[C, D]): EsoParseRes[C, D] = f(res)
  def toOption: Option[B] = Some(res)
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
  def withConditioning(f: A => A): OrderedParser[A, B] = ConditioningParser(this, f)
  
  def <+>(p: OrderedParser[A, B]): OrderedMultiParser[A, B] = p match{
    case OrderedMultiParser(ps) => OrderedMultiParser(this +: ps)
    case _ => OrderedMultiParser(Vector(this, p))}
}