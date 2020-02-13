package parsers

import common.{EsoExcep, EsoObj}

import scala.util.{Failure, Success, Try}

trait EsoParseRes[+A, +B]{
  def passed: Boolean
  def start: Int
  def end: Int
  def map[C](f: B => C): EsoParseRes[A, C]
  def flatMap[C, D](f: B => EsoParseRes[C, D]): EsoParseRes[C, D]
  def mapAll[C, D](f: (B, A, Int, Int) => (D, C, Int, Int)): EsoParseRes[C, D]
  def flatMapAll[C, D](f: (B, A, Int, Int) => EsoParseRes[C, D]): EsoParseRes[C, D]
  def toOption: Option[B]
  def get: B
  def next: A
  
  def getOrElse[U >: B](default: U): U = toOption match{
    case Some(res) => res
    case None => default}
  def toTry(err: String = "Parse Failed"): Try[B] = toOption match{
    case Some(res) => Success(res)
    case None => Failure(EsoExcep(err))}
}
object EsoParseFail extends EsoParseRes[Nothing, Nothing]{
  def passed: Boolean = false
  def start: Int = -1
  def end: Int = -1
  def map[C](f: Nothing => C): EsoParseRes[Nothing, Nothing] = EsoParseFail
  def flatMap[C, D](f: Nothing => EsoParseRes[C, D]): EsoParseRes[Nothing, Nothing] = EsoParseFail
  def mapAll[C, D](f: (Nothing, Nothing, Int, Int) => (D, C, Int, Int)): EsoParseRes[C, D] = EsoParseFail
  def flatMapAll[C, D](f: (Nothing, Nothing, Int, Int) => EsoParseRes[C, D]): EsoParseRes[C, D] = EsoParseFail
  def toOption: Option[Nothing] = None
  def get: Nothing = throw EsoExcep("No Such Element")
  def next: Nothing = throw EsoExcep("No Such Element")
}
case class EsoParsed[+A, +B](res: B, rem: A, start: Int, end: Int) extends EsoParseRes[A, B]{
  def passed: Boolean = true
  def map[C](f: B => C): EsoParseRes[A, C] = EsoParsed(f(res), rem, start, end)
  def flatMap[C, D](f: B => EsoParseRes[C, D]): EsoParseRes[C, D] = f(res)
  def mapAll[C, D](f: (B, A, Int, Int) => (D, C, Int, Int)): EsoParseRes[C, D] = f(res, rem, start, end) match{
    case (nres, nrem, ns, ne) => EsoParsed(nres, nrem, ns, ne)}
  def flatMapAll[C, D](f: (B, A, Int, Int) => EsoParseRes[C, D]): EsoParseRes[C, D] = f(res, rem, start, end)
  def toOption: Option[B] = Some(res)
  def get: B = res
  def next: A = rem
}

abstract class EsoParser[A, B] extends (A => EsoParseRes[A, B]) with EsoObj{
  def apply(inp: A): EsoParseRes[A, B]
  def parseOne(inp: A): B = apply(inp) match{
    case EsoParsed(res, _, _, _) => res
    case _ => throw EsoExcep("Parse Failed")}
  
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
  
  def map[C](f: B => C): EsoParser[A, C] = MappedParser(this, f)
  def withConditioning(f: A => A): EsoParser[A, B] = ConditioningParser(this, f)
  
  def andThen[C](q: EsoParser[B, C]): EsoParser[A, C] = ChainedParser(this, q)
  
  def toBulk: BulkParser[A, B] = BulkParser(this)
  
  def |(q: EsoParser[A, B]): AlternativeParser[A, B] = q match{
    case AlternativeParser(qs) => AlternativeParser(this +: qs)
    case _ => AlternativeParser(Vector(this, q))}
  
  def <+>(p: EsoParser[A, B]): LongestMatchAlternativeParser[A, B] = p match{
    case LongestMatchAlternativeParser(ps) => LongestMatchAlternativeParser(this +: ps)
    case _ => LongestMatchAlternativeParser(Vector(this, p))}
  
  def <~[U](q: => EsoParser[A, U]): LeftImplicationParser[A, B, U] = LeftImplicationParser(this, q)
  def ~>[U](q: => EsoParser[A, U]): RightImplicationParser[A, B, U] = RightImplicationParser(this, q)
  
  def ~[U](q: => EsoParser[A, U]): SequentialParser[A, B, U] = SequentialParser(this, q)
}