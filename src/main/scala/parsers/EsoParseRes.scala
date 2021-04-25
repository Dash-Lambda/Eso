package parsers

import common.EsoExcep

import scala.util.control.TailCalls.{TailRec, done, tailcall}
import scala.util.{Failure, Success, Try}
object ParserCalls{
  abstract class ParseTramp[+A] {
    def map[U](f: A => U): ParseTramp[U] = flatMap(a => ParseCall(() => Parsed(f(a))))
    def flatMap[U](f: A => ParseTramp[U]): ParseTramp[U] = ParseCont(this, f)
    def orElse[U >: A](alt: => ParseTramp[U]): ParseTramp[U] = ParseAlt(this, ParseCall(() => alt))
    def resume: ParseTramp[A]
    def result: ParseRes[A] = {
      @annotation.tailrec
      def rec(a: ParseTramp[A], ac: Set[ParseTramp[A]]): ParseRes[A] = a match{
        case r: ParseRes[A] => r
        case _ => rec(a.resume, ac)}
      rec(this, Set())}
  }
  
  case class ParseCall[A](next: () => ParseTramp[A]) extends ParseTramp[A] {
    override def toString: String = s"Call(() => ${next()})"
    def resume: ParseTramp[A] = next()}
  
  case class ParseCont[A, B](v: ParseTramp[A], fun: A => ParseTramp[B]) extends ParseTramp[B] {
    override def toString: String = s"Cont($v)"
    def resume: ParseTramp[B] = {
      v match{
        case Parsed(res) => fun(res)
        case ParseFail => ParseFail
        case ParseCall(t) => ParseCont(t(), fun)
        case ParseAlt(a, b) => a match{
          case Parsed(res) => ParseAlt(fun(res), ParseCont(b, fun))
          case ParseFail => ParseCont(b, fun)
          case ParseCall(t) => ParseCont(ParseAlt(t(), b), fun)
          case ParseAlt(c, d) => ParseCont(ParseAlt(c, ParseAlt(d, b)), fun)
          case _: ParseCont[_, _] => ParseBack.curry(a)(x => ParseCont(ParseAlt(x, b), fun)) //ParseCont(v.resume, fun)
          case ParseBack(c, h) => ParseBack.curry(c)(x => ParseCont(ParseAlt(h(x), b), fun))}
        case _: ParseCont[_, _] => ParseBack.curry(v)(x => ParseCont(x, fun)) //ParseCont(v.resume, fun)
        case ParseBack(b, g) => ParseBack.curry(b)(x => ParseCont(g(x), fun))}}
  }
  
  case class ParseAlt[A](a: ParseTramp[A], b: ParseTramp[A]) extends ParseTramp[A] {
    override def toString: String = s"Alt($a, $b)"
    override def orElse[U >: A](alt: => ParseTramp[U]): ParseTramp[U] = ParseAlt(a, ParseAlt(b, ParseCall(() => alt)))
    def resume: ParseTramp[A] = {
      a match{
        case Parsed(res) => Parsed(res)
        case ParseFail => b
        case ParseCall(t) => ParseAlt(t(), b)
        case ParseAlt(c, d) => ParseAlt(c, ParseAlt(d, b))
        case _: ParseCont[_, _] => ParseBack.curry(a)(x => ParseAlt(x, b)) // ParseAlt(a.resume, b)
        case ParseBack(c, f) => ParseBack.curry(c)(x => ParseAlt(f(x), b))}}
  }
  
  case class ParseBack[A, B](a: ParseTramp[A], f: ParseTramp[A] => ParseTramp[B]) extends ParseTramp[B]{
    def resume: ParseTramp[B] = a match{
      case Parsed(res) => f(Parsed(res))
      case ParseFail => f(ParseFail)
      case ParseCall(t) => ParseBack(t(), f)
      case ParseAlt(b, c) => f(ParseAlt(b, c))
      case ParseCont(b, g) => b match{
        case Parsed(res) => f(g(res))
        case ParseFail => f(ParseFail)
        case ParseCall(t) => ParseBack(ParseCont(t(), g), f)
        case ParseAlt(c, d) => c match{
          case Parsed(res) => f(ParseAlt(g(res), ParseCont(d, g)))
          case ParseFail => f(ParseCont(d, g))
          case ParseAlt(x, y) => ParseBack(ParseCont(ParseAlt(x, ParseAlt(y, d)), g), f)
          case _ => ParseBack.curry(c)(x => ParseBack(ParseCont(ParseAlt(x, d), g), f))}
        case ParseCont(c, h) => ParseBack.curry(ParseCont(c, h))(x => ParseBack(ParseCont(x, g), f))
        case ParseBack(c, h) => ParseBack.curry(c)(x => ParseBack(ParseCont(h(x), g), f))}
      case ParseBack(b, g) => ParseBack.curry(b)(x => ParseBack(g(x), f))}
  }
  object ParseBack{
    def curry[A, B](a: ParseTramp[A])(f: ParseTramp[A] => ParseTramp[B]): ParseBack[A, B] = ParseBack(a, f)
  }
}

trait ParseRes[+A] extends ParserCalls.ParseTramp[A]{
  def resume: ParserCalls.ParseTramp[A] = this
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