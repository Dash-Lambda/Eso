package parsers

import common.EsoExcep
import scala.util.{Failure, Success, Try}

object ParserCalls{
  def delayParse[A](p: => EsoParser[A], inp: String, ind: Int): ParserCalls.ParseTramp[(A, String, Int, Int)] = ParserCalls.Eval(p, inp, ind)
  
  trait ParseTramp[+A] {
    final def map[U](f: A => U): ParseTramp[U] = flatMap(a => Call(() => Parsed(f(a))))
    final def flatMap[U](f: A => ParseTramp[U]): ParseTramp[U] = FMap(this, f)
    final def orElse[U >: A](alt: => ParseTramp[U]): ParseTramp[U] = Alt(this, Call(() => alt))
    final def withCont[C](f: ParseNode[A] => ParseTramp[C]): ParseTramp[C] = Cont(this, f)
    def result: ParseRes[A] = {
      @annotation.tailrec
      def rec(a: ParseTramp[A], ac: Set[ParseTramp[A]]): ParseRes[A] = {
        //println(s"- Rec: $a")
        //Thread.sleep(10)
        a match{ // ac it here for the purpose of memoization, not used yet and I don't wanna take it out
          // Result
          case Parsed(res) => Parsed(res)
          case ParseFail => ParseFail
    
          // Delayed Call
          case Call(t) => rec(t(), ac)
    
          // Delayed Parser Application
          case Eval(p, inp, ind) => rec(p(inp, ind), ac)
    
          // OrElse
          case Alt(Parsed(res), _) => Parsed(res)
          case Alt(ParseFail, b) => rec(b, ac)
          case Alt(Eval(p, inp, ind), b) => rec(Alt(p(inp, ind), b), ac)
          case Alt(Alt(a, b), c) => rec(Alt(a, Alt(b, c)), ac)
          case Alt(a, b) => rec(a.withCont(x => Alt(x, b)), ac)
    
          // FlatMap
          case FMap(Parsed(res), f) => rec(f(res), ac)
          case FMap(ParseFail, _) => ParseFail
          case FMap(Eval(p, inp, ind), f) => rec(FMap(p(inp, ind), f), ac)
          case FMap(Alt(Parsed(res), b), f) => rec(Alt(f(res), FMap(b, f)), ac)
          case FMap(Alt(ParseFail, b), f) => rec(FMap(b, f), ac)
          case FMap(Alt(Eval(p, inp, ind), b), f) => rec(FMap(Alt(p(inp, ind), b), f), ac)
          case FMap(Alt(FMap(a, g), b), f) => rec(FMap(a, g).withCont(x => FMap(Alt(x, b), f)), ac)
          case FMap(Alt(a, b), f) => rec(Alt(a, b).withCont(x => FMap(x, f)), ac)
          case FMap(a, f) => rec(a.withCont(x => FMap(x, f)), ac)
    
          // Continue (Result)
          case Cont(Parsed(res), f) => rec(f(Parsed(res)), ac)
          case Cont(ParseFail, f) => rec(f(ParseFail), ac)
    
          // Continue (Delayed Call)
          case Cont(Call(t), f) => rec(Cont(t(), f), ac)
    
          // Continue (Delayed Parser Application)
          case Cont(Eval(p, inp, ind), f) => rec(Cont(p(inp, ind), f), ac)
    
          // Continue (OrElse)
          case Cont(Alt(Parsed(res), b), f) => rec(Alt(f(Parsed(res)), Cont(b, f)), ac)
          case Cont(Alt(ParseFail, b), f) => rec(Cont(b, f), ac)
          case Cont(Alt(Call(t), b), f) => rec(f(Alt(t(), b)), ac)
          case Cont(Alt(Eval(p, inp, ind), b), f) => rec(f(Alt(p(inp, ind), b)), ac)
          case Cont(Alt(Alt(a, b), c), f) => rec(f(Alt(a, Alt(b, c))), ac)
          case Cont(Alt(a, b), f) => rec(f(Alt(a, b)), ac)
    
          // Continue (FlatMap)
          case Cont(FMap(Parsed(res), g), f) => rec(Cont(g(res), f), ac)
          case Cont(FMap(ParseFail, _), f) => rec(f(ParseFail), ac)
          case Cont(FMap(Call(t), g), f) => rec(Cont(FMap(t(), g), f), ac)
          case Cont(FMap(Eval(p, inp, ind), g), f) => rec(Cont(FMap(p(inp, ind), g), f), ac)
          case Cont(FMap(FMap(a, h), g), f) => rec(FMap(a, h).withCont(x => Cont(FMap(x, g), f)), ac)
          case Cont(FMap(Cont(a, h), g), f) => rec(a.withCont(x => Cont(FMap(h(x), g), f)), ac)
          case Cont(FMap(Alt(Parsed(res), b), g), f) => rec(f(Alt(g(res), FMap(b, g))), ac)
          case Cont(FMap(Alt(ParseFail, b), g), f) => rec(Cont(FMap(b, g), f), ac)
          case Cont(FMap(Alt(Alt(a, b), c), g), f) => rec(Cont(FMap(Alt(a, Alt(b, c)), g), f), ac)
          case Cont(FMap(Alt(a, b), g), f) => rec(a.withCont(x => Cont(FMap(Alt(x, b), g), f)), ac)
    
          // Continue (Continue)
          case Cont(Cont(a, g), f) => rec(a.withCont(x => Cont(g(x), f)), ac)}}
      rec(this, Set())}}
  
  trait ParseNode[+A] extends ParseTramp[A]
  
  protected case class Eval[U](parser: (String, Int) => ParseTramp[U], inp: String, ind: Int) extends ParseTramp[U]{
    override def toString: String = s"App($parser, $inp, $ind)"}
  
  protected case class Call[A](next: () => ParseTramp[A]) extends ParseTramp[A] {
    override def toString: String = s"Call(() => ${next()})"}
  
  protected case class FMap[A, B](a: ParseTramp[A], f: A => ParseTramp[B]) extends ParseTramp[B] {
    override def toString: String = s"FMap($a)"}
  
  protected case class Alt[A](a: ParseTramp[A], b: ParseTramp[A]) extends ParseNode[A] {
    override def toString: String = s"Alt($a, $b)"}
  
  case class Cont[A, B](a: ParseTramp[A], f: ParseNode[A] => ParseTramp[B]) extends ParseTramp[B]{
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