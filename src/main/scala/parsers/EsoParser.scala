package parsers

import common.EsoObj

import scala.util.matching.Regex

import ParserCalls._

trait EsoParser[+A] extends ((String, Int) => ParseTramp[(A, String, Int, Int)]) with (String => ParseRes[(A, String, Int, Int)]) with EsoObj{
  import NewParsers._
  def apply(inp: String): ParseRes[(A, String, Int, Int)] = apply(inp, 0).result
  
  def matches(inp: String): Boolean = apply(inp).passed
  
  def iterator(inp: String): Iterator[(A, String, Int, Int)] = Iterator.unfold(0: Int)(i => apply(inp, i).result.get.map(r => (r, r._4)))
  
  def <|[B](q: => EsoParser[B]): EsoParser[A] = lcond(this, q)
  def |>[B](q: => EsoParser[B]): EsoParser[B] = rcond(this, q)
  def &>[B](q: => EsoParser[B]): EsoParser[B] = rimp(this, q)
  def <&[B](q: => EsoParser[B]): EsoParser[A] = limp(this, q)
  def <&>[B](q: => EsoParser[B]): EsoParser[(A, B)] = prod(this, q)
  
  def |[B >: A](q: => EsoParser[B]): EsoParser[B] = alt(this, q)
  def ||[B >: A](q: => EsoParser[B]): EsoParser[B] = earliest(this, q)
  def |||[B >: A](q: => EsoParser[B]): EsoParser[B] = longest(this, q)
  
  def * : EsoParser[Vector[A]] = all(this)
  def + : EsoParser[Vector[A]] = all(this, 1)
  
  def onlyIf(cond: (A, String, Int, Int) => Boolean): EsoParser[A] = conditional(this)(cond)
  
  def ^^[U](f: A => U): EsoParser[U] = map(f)
  def ^^^[U](v: => U): EsoParser[U] = map(_ => v)
  def map[U](f: A => U): EsoParser[U] = (inp, ind) => apply(inp, ind) map {case (r, i, s, e) => (f(r), i, s, e)}
  def flatMap[U](f: A => EsoParser[U]): EsoParser[U] = (inp, ind) => apply(inp, ind) flatMap{
    case (r, i, s, e) =>
      f(r)(i, e) map{
        case (fr, fi, _, fe) =>
          (fr, fi, s, fe)}}
}
object EsoParser{
  import parsers.NewParsers.{regexparse, stringparse}
  def empty[A](v: => A): EsoParser[A] = (inp, ind) => Parsed((v, inp, ind, ind))
  def S(str: String): EsoParser[String] = stringparse(str)
  def R(regex: Regex): EsoParser[String] = regexparse(regex)
  def R(regex: String): EsoParser[String] = regexparse(regex.r)
}

object NewParsers{
  def regexparse(reg: Regex): EsoParser[String] = {
    (inp, ind) =>
      val matcher = reg.pattern.matcher(inp)
      matcher.region(ind, inp.length)
      if(matcher.find)
        if(matcher.groupCount > 0) Parsed(((1 to matcher.groupCount).map(matcher.group).mkString, inp, matcher.start, matcher.end))
        else Parsed(matcher.group(), inp, matcher.start, matcher.end)
      else ParseFail}
  def stringparse(str: String): EsoParser[String] =
    (inp, ind) =>
      if(inp.startsWith(str, ind)) Parsed((str, inp, ind, ind + str.length))
      else ParseFail
  
  def alt[A, B >: A](parser1: => EsoParser[A], parser2: => EsoParser[B]): EsoParser[B] = {
    lazy val p = parser1
    lazy val q = parser2
    (inp, ind) =>
      p(inp, ind) orElse q(inp, ind)
  }
  
  def prod[A, B](parser1: => EsoParser[A], parser2: => EsoParser[B]): EsoParser[(A, B)] = {
    lazy val p = parser1
    lazy val q = parser2
    (inp, ind) =>
      p(inp, ind) flatMap {
        case (pr, pi, ps, pe) => q(pi, pe) map {
          case (qr, qi, _, qe) => ((pr, qr), qi, ps, qe)}}}
  
  // Higher-order combinators
  def concat(p: => EsoParser[String], q: => EsoParser[String]): EsoParser[String] = prod(p, q) map {case (a, b) => a + b}
  
  def longest[A](parser1: => EsoParser[A], parser2: => EsoParser[A]): EsoParser[A] = {
    lazy val p = parser1
    lazy val q = parser2
    (inp, ind) =>
      p(inp, ind) flatMap{
        case (pr, pi, ps, pe) =>
          q(inp, ind) flatMap{
            case (qr, qi, qs, qe) =>
              val qres = Parsed((qr, qi, qs, qe))
              val pres = Parsed((pr, pi, ps, pe))
              if((pe - ps) >= (qe - qs)) pres orElse qres
              else qres orElse pres} orElse Parsed((pr, pi, ps, pe))} orElse q(inp, ind)}
  
  def earliest[A](parser1: => EsoParser[A], parser2: => EsoParser[A]): EsoParser[A] = {
    lazy val p = parser1
    lazy val q = parser2
    (inp, ind) =>
      p(inp, ind) flatMap{
        case (pr, pi, ps, pe) =>
          q(inp, ind) flatMap{
            case (qr, qi, qs, qe) =>
              val qres = Parsed((qr, qi, qs, qe))
              val pres = Parsed((pr, pi, ps, pe))
              if(pe <= qe) pres orElse qres
              else qres orElse pres} orElse Parsed((pr, pi, ps, pe))} orElse q(inp, ind)}
  
  def all[A](parser: => EsoParser[A], num: Int = 0): EsoParser[Vector[A]] = {
    lazy val p = parser
    lazy val base = EsoParser.empty(Vector[A]())
    lazy val init: EsoParser[Vector[A]] =
      Vector.fill(num)(p).foldLeft(base){
        case (ac, np) =>
          prod(ac, np) map {
            case (vec, e) => vec :+ e}}
    lazy val s: EsoParser[Vector[A]] = ((p <&> s) map (x => x._1 +: x._2)) | base
    (init <&> s) map {case (a, b) => a ++ b}}
  
  def into[A](parser1: => EsoParser[String], parser2: => EsoParser[A]): EsoParser[A] = {
    lazy val p = parser1
    lazy val q = parser2
    (inp, ind) =>
      p(inp, ind) flatMap {
        case (pr, pi, ps, pe) =>
          q(pr, 0) map {
            case (qr, _, _, _) =>
              (qr, pi, ps, pe)}}}
  
  def conditional[A](parser: => EsoParser[A])(cond: (A, String, Int, Int) => Boolean): EsoParser[A] = {
    lazy val p = parser
    (inp, ind) =>
      p(inp, ind) flatMap{
        case (pr, pi, ps, pe) =>
          if(cond(pr, pi, ps, pe)) Parsed((pr, pi, ps, pe))
          else ParseFail}}
  
  def limp[A, B](p: => EsoParser[A], q: => EsoParser[B]): EsoParser[A] = prod(p, q) map {case (a, _) => a}
  def rimp[A, B](p: => EsoParser[A], q: => EsoParser[B]): EsoParser[B] = prod(p, q) map {case (_, b) => b}
  def lcond[A, B](parser1: => EsoParser[A], parser2: => EsoParser[B]): EsoParser[A] = {
    lazy val p = parser1
    lazy val q = parser2
    (inp, ind) =>
      p(inp, ind) flatMap{
        case (pr, pi, ps, pe) =>
          q(pi, pe) map(_ => (pr, pi, ps, pe))}}
  def rcond[A, B](parser1: => EsoParser[A], parser2: => EsoParser[B]): EsoParser[B] = {
    lazy val p = parser1
    lazy val q = parser2
    (inp, ind) =>
      p(inp, ind) flatMap{
        case (_, pi, _, pe) => q(pi, pe)}}
  
  def const[A, B](p: => EsoParser[A], v: => B): EsoParser[B] = p map (_ => v)
}