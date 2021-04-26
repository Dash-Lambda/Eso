package parsers

import common.EsoExcep
import scala.util.{Failure, Success, Try}

object ParserCalls{
  def delayParse[A](p: => EsoParser[A], inp: String, ind: Int): ParserCalls.ParseTramp[(A, String, Int, Int)] = ParserCalls.ParseApp(p, inp, ind)
  
  trait ParseNode[+A] extends ParseTramp[A]
  trait ParseTramp[+A] {
    final def map[U](f: A => U): ParseTramp[U] = flatMap(a => ParseCall(() => Parsed(f(a))))
    final def flatMap[U](f: A => ParseTramp[U]): ParseTramp[U] = ParseFMap(this, f)
    final def orElse[U >: A](alt: => ParseTramp[U]): ParseTramp[U] = ParseAlt(this, ParseCall(() => alt))
    final def withCont[C](f: ParseNode[A] => ParseTramp[C]): ParseTramp[C] = ParseCont(this, f)
    def result: ParseRes[A] = {
      @annotation.tailrec
      def rec(a: ParseTramp[A], ac: Set[ParseTramp[A]]): ParseRes[A] = a match{ // ac it here for the purpose of memoization, not used yet and I don't wanna take it out
        // Result
        case Parsed(res) => Parsed(res)
        case ParseFail => ParseFail
        
        // Delayed Call
        case ParseCall(t) => rec(t(), ac)
        
        // Delayed Parser Application
        case ParseApp(p, inp, ind) => rec(p(inp, ind), ac)
        
        // OrElse
        case ParseAlt(Parsed(res), _) => Parsed(res)
        case ParseAlt(ParseFail, b) => rec(b, ac)
        case ParseAlt(ParseAlt(a, b), c) => rec(ParseAlt(a, ParseAlt(b, c)), ac)
        case ParseAlt(a, b) => rec(a.withCont(x => ParseAlt(x, b)), ac)
        
        // FlatMap
        case ParseFMap(Parsed(res), f) => rec(f(res), ac)
        case ParseFMap(ParseFail, _) => ParseFail
        case ParseFMap(ParseAlt(Parsed(res), b), f) => rec(ParseAlt(f(res), ParseFMap(b, f)), ac)
        case ParseFMap(ParseAlt(ParseFail, b), f) => rec(ParseFMap(b, f), ac)
        case ParseFMap(ParseAlt(ParseFMap(a, g), b), f) => rec(ParseFMap(a, g).withCont(x => ParseFMap(ParseAlt(x, b), f)), ac)
        case ParseFMap(ParseAlt(a, b), f) => rec(ParseAlt(a, b).withCont(x => ParseFMap(x, f)), ac)
        case ParseFMap(a, f) => rec(a.withCont(x => ParseFMap(x, f)), ac)
        
        // Continue (Result)
        case ParseCont(Parsed(res), f) => rec(f(Parsed(res)), ac)
        case ParseCont(ParseFail, f) => rec(f(ParseFail), ac)
        
        // Continue (Delayed Call)
        case ParseCont(ParseCall(t), f) => rec(ParseCont(t(), f), ac)
        
        // Continue (Delayed Parser Application)
        case ParseCont(ParseApp(p, inp, ind), f) => rec(ParseCont(p(inp, ind), f), ac)
        
        // Continue (OrElse)
        case ParseCont(ParseAlt(Parsed(res), b), f) => rec(ParseAlt(f(Parsed(res)), ParseCont(b, f)), ac)
        case ParseCont(ParseAlt(ParseFail, b), f) => rec(ParseCont(b, f), ac)
        case ParseCont(ParseAlt(ParseCall(t), b), f) => rec(f(ParseAlt(t(), b)), ac)
        case ParseCont(ParseAlt(ParseApp(p, inp, ind), b), f) => rec(f(ParseAlt(p(inp, ind), b)), ac)
        case ParseCont(ParseAlt(ParseAlt(a, b), c), f) => rec(f(ParseAlt(a, ParseAlt(b, c))), ac)
        case ParseCont(ParseAlt(a, b), f) => rec(f(ParseAlt(a, b)), ac)
        
        // Continue (FlatMap)
        case ParseCont(ParseFMap(Parsed(res), g), f) => rec(ParseCont(g(res), f), ac)
        case ParseCont(ParseFMap(ParseFail, _), f) => rec(f(ParseFail), ac)
        case ParseCont(ParseFMap(ParseCall(t), g), f) => rec(ParseCont(ParseFMap(t(), g), f), ac)
        case ParseCont(ParseFMap(ParseApp(p, inp, ind), g), f) => rec(ParseCont(ParseFMap(p(inp, ind), g), f), ac)
        case ParseCont(ParseFMap(ParseFMap(a, h), g), f) => rec(ParseFMap(a, h).withCont(x => ParseCont(ParseFMap(x, g), f)), ac)
        case ParseCont(ParseFMap(ParseCont(a, h), g), f) => rec(a.withCont(x => ParseCont(ParseFMap(h(x), g), f)), ac)
        case ParseCont(ParseFMap(ParseAlt(Parsed(res), b), g), f) => rec(f(ParseAlt(g(res), ParseFMap(b, g))), ac)
        case ParseCont(ParseFMap(ParseAlt(ParseFail, b), g), f) => rec(ParseCont(ParseFMap(b, g), f), ac)
        case ParseCont(ParseFMap(ParseAlt(ParseAlt(a, b), c), g), f) => rec(ParseCont(ParseFMap(ParseAlt(a, ParseAlt(b, c)), g), f), ac)
        case ParseCont(ParseFMap(ParseAlt(a, b), g), f) => rec(a.withCont(x => ParseCont(ParseFMap(ParseAlt(x, b), g), f)), ac)
        
        // Continue (Continue)
        case ParseCont(ParseCont(a, g), f) => rec(a.withCont(x => ParseCont(g(x), f)), ac)
      }
      rec(this, Set())
    }
  }
  
  protected case class ParseApp[U](parser: (String, Int) => ParseTramp[U], inp: String, ind: Int) extends ParseTramp[U]{
    override def toString: String = s"App($parser, $inp, $ind)"}
  
  protected case class ParseCall[A](next: () => ParseTramp[A]) extends ParseTramp[A] {
    override def toString: String = s"Call(() => ${next()})"}
  
  protected case class ParseFMap[A, B](a: ParseTramp[A], f: A => ParseTramp[B]) extends ParseTramp[B] {
    override def toString: String = s"FMap($a)"}
  
  protected case class ParseAlt[A](a: ParseTramp[A], b: ParseTramp[A]) extends ParseNode[A] {
    override def toString: String = s"Alt($a, $b)"}
  
  case class ParseCont[A, B](a: ParseTramp[A], f: ParseNode[A] => ParseTramp[B]) extends ParseTramp[B]{
    override def toString: String = s"Cont($a)"}
}

trait ParseRes[+A] extends ParserCalls.ParseNode[A]{
  def passed: Boolean = get.nonEmpty
  def getOrElse[B >: A](alt: => B): B = get.getOrElse(alt)
  def toTry(str: String = "Parse Failure"): Try[A] = get match{
    case Some(res) => Success(res)
    case _ => Failure(EsoExcep(str))}
  def mapped[U](f: A => U): ParseRes[U] = flatMapped(r => Parsed(f(r)))
  
  def get: Option[A]
  def flatMapped[U](f: A => ParseRes[U]): ParseRes[U]
}

case class Parsed[A](res: A) extends ParseRes[A]{
  def get: Option[A] = Some(res)
  def flatMapped[U](f: A => ParseRes[U]): ParseRes[U] = f(res)
}
object ParseFail extends ParseRes[Nothing]{
  def get: Option[Nothing] = None
  def flatMapped[U](f: Nothing => ParseRes[U]): ParseRes[U] = ParseFail
  override def toString: String = s"ParseFail"
}