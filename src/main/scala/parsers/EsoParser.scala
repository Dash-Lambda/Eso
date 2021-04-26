package parsers

import common.EsoObj

import scala.util.matching.Regex

import ParserCalls._

trait EsoParser[+A] extends ((String, Int) => ParseTramp[(A, String, Int, Int)]) with (String => ParseRes[(A, String, Int, Int)]) with EsoObj{
  override def toString: String = s"EsoParser[A]"
  import CombinatorFuncs._
  def apply(inp: String): ParseRes[(A, String, Int, Int)] = apply(inp, 0).result
  def delay(inp: String, ind: Int): ParseTramp[(A, String, Int, Int)] = ParserCalls.delayParse(this, inp, ind)
  
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
  def map[U](f: A => U): EsoParser[U] = mapped(this)(f)
  def flatMap[U](f: A => EsoParser[U]): EsoParser[U] = (inp, ind) => delay(inp, ind) flatMap{
    case (r, i, s, e) =>
      f(r).delay(i, e) map{
        case (fr, fi, _, fe) =>
          (fr, fi, s, fe)}}
  def flatMapAll[U](f: (A, String, Int, Int) => EsoParser[U]): EsoParser[U] = (inp, ind) => delay(inp, ind) flatMap{
    case (r, i, s, e) =>
      f(r, i, s, e).delay(i, e)}
}
object EsoParser{
  import CombinatorFuncs.{RegexParse, StringParse}
  def empty[A](v: => A): EsoParser[A] = (inp, ind) => Parsed((v, inp, ind, ind))
  def S(str: String): EsoParser[String] = StringParse(str)
  def R(regex: Regex): EsoParser[String] = RegexParse(regex)
  def R(regex: String): EsoParser[String] = RegexParse(regex.r)
}

object CombinatorFuncs{
  // Primitives (combinators which other combinators are made from)
  case class RegexParse(reg: Regex) extends EsoParser[String]{
    def apply(inp: String, ind: Int): ParseTramp[(String, String, Int, Int)] = {
      val matcher = reg.pattern.matcher(inp)
      matcher.region(ind, inp.length)
      if(matcher.find)
        if(matcher.groupCount > 0) Parsed(((1 to matcher.groupCount).map(matcher.group).mkString, inp, matcher.start, matcher.end))
        else Parsed(matcher.group(), inp, matcher.start, matcher.end)
      else ParseFail}}
  
  case class StringParse(str: String) extends EsoParser[String]{
    def apply(inp: String, ind: Int): ParseTramp[(String, String, Int, Int)] = {
      if(inp.startsWith(str, ind)) Parsed((str, inp, ind, ind + str.length))
      else ParseFail}
    override def toString: String = s"S($str)"}
  
  class AltParse[A](parser1: => EsoParser[A], parser2: => EsoParser[A]) extends EsoParser[A]{
    private lazy val p = parser1
    private lazy val q = parser2
    def apply(inp: String, ind: Int): ParseTramp[(A, String, Int, Int)] = {
      ParserCalls.delayParse(p, inp, ind) orElse ParserCalls.delayParse(q, inp, ind)}}
  
  class SortedAltParse[A](parser1: => EsoParser[A], parser2: => EsoParser[A], comp: ((A, String, Int, Int), (A, String, Int, Int)) => Boolean) extends EsoParser[A]{
    private lazy val p = parser1
    private lazy val q = parser2
    def apply(inp: String, ind: Int): ParseTramp[(A, String, Int, Int)] = {
      p.delay(inp, ind) flatMap{
        case (pr, pi, ps, pe) =>
          q.delay(inp, ind) flatMap{
            case (qr, qi, qs, qe) =>
              val pres = Parsed((pr, pi, ps, pe))
              val qres = Parsed((qr, qi, qs, qe))
              if(comp(pres.res, qres.res)) pres orElse qres
              else qres orElse pres} orElse Parsed((pr, pi, ps, pe))} orElse q.delay(inp, ind)}}
  
  class ProdParse[A, B](parser1: => EsoParser[A], parser2: => EsoParser[B]) extends EsoParser[(A, B)]{
    private lazy val p = parser1
    private lazy val q = parser2
    def apply(inp: String, ind: Int): ParseTramp[((A, B), String, Int, Int)] = {
      p.delay(inp, ind) flatMap {
        case (pr, pi, ps, pe) => q.delay(pi, pe) map {
          case (qr, qi, _, qe) => ((pr, qr), qi, ps, qe)}}}}
  
  class IntoParse[A](parser1: => EsoParser[String], parser2: => EsoParser[A]) extends EsoParser[A]{
    private lazy val p = parser1
    private lazy val q = parser2
    def apply(inp: String, ind: Int): ParseTramp[(A, String, Int, Int)] = {
      p.delay(inp, ind) flatMap {
        case (pr, pi, ps, pe) =>
          q.delay(pr, 0) map {
            case (qr, _, _, _) =>
              (qr, pi, ps, pe)}}}}
  
  class ConditionParse[A](parser: => EsoParser[A], cond: (A, String, Int, Int) => Boolean) extends EsoParser[A]{
    private lazy val p = parser
    def apply(inp: String, ind: Int): ParseTramp[(A, String, Int, Int)] = {
      p.delay(inp, ind) flatMap{
        case (pr, pi, ps, pe) =>
          if(cond(pr, pi, ps, pe)) Parsed((pr, pi, ps, pe))
          else ParseFail}}}
  
  class AllFlatMapParse[A, B](parser1: => EsoParser[A], comp: (A, String, Int, Int) => ParseTramp[(B, String, Int, Int)]) extends EsoParser[B]{
    private lazy val p = parser1
    def apply(inp: String, ind: Int): ParseTramp[(B, String, Int, Int)] = {
      p.delay(inp, ind) flatMap{
        case (pr, pi, ps, pe) =>
          comp(pr, pi, ps, pe)}}}
  
  class LCondParse[A, B](parser1: => EsoParser[A], parser2: => EsoParser[B]) extends EsoParser[A]{
    private lazy val p = parser1
    private lazy val q = parser2
    def apply(inp: String, ind: Int): ParseTramp[(A, String, Int, Int)] = {
      p.delay(inp, ind) flatMap {
        case (pr, pi, ps, pe) => q.delay(pi, pe) map (_ => (pr, pi, ps,pe))}}}
  
  class RCondParse[A, B](parser1: => EsoParser[A], parser2: => EsoParser[B]) extends EsoParser[B]{
    private lazy val p = parser1
    private lazy val q = parser2
    def apply(inp: String, ind: Int): ParseTramp[(B, String, Int, Int)] = {
      p.delay(inp, ind) flatMap {
        case (_, pi, _, pe) => q.delay(pi, pe)}}}
  
  def alt[A](p: => EsoParser[A], q: => EsoParser[A]): EsoParser[A] = new AltParse(p, q)
  def sortAlt[A](p: => EsoParser[A], q: => EsoParser[A])(comp: ((A, String, Int, Int), (A, String, Int, Int)) => Boolean): EsoParser[A] = new SortedAltParse(p, q, comp)
  def prod[A, B](p: => EsoParser[A], q: => EsoParser[B]): EsoParser[(A, B)] = new ProdParse(p, q)
  def into[A](p: => EsoParser[String], q: => EsoParser[A]): EsoParser[A] = new IntoParse(p, q)
  def conditional[A](p: => EsoParser[A])(cond: (A, String, Int, Int) => Boolean): EsoParser[A] = new ConditionParse(p, cond)
  def allFlatMapped[A, B](p: => EsoParser[A])(comp: (A, String, Int, Int) => ParseTramp[(B, String, Int, Int)]): EsoParser[B] = new AllFlatMapParse(p, comp)
  def mapped[A, B](p: => EsoParser[A])(f: A => B): EsoParser[B] = {
    def fun(r: A, i: String, s: Int, e: Int): ParseTramp[(B, String, Int, Int)] = Parsed((f(r), i, s, e)) // Maybe Scala 3 will have more robust lambda typing...
    new AllFlatMapParse(p, fun)}
  def lcond[A, B](p: => EsoParser[A], q: => EsoParser[B]): EsoParser[A] = new LCondParse(p, q)
  def rcond[A, B](p: => EsoParser[A], q: => EsoParser[B]): EsoParser[B] = new RCondParse(p, q)
  
  // Derived combinators
  import scala.collection.{IterableOps, IndexedSeqOps}
  def append[A, SS[_] <: IndexedSeq[_], S, B >: A](p: => EsoParser[IndexedSeqOps[A, SS, S]], q: => EsoParser[B]): EsoParser[SS[B]] = prod(p, q) map {case (v, e) => v :+ e}
  def prepend[A, SS[_] <: IndexedSeq[_], S, B >: A](p: => EsoParser[B], q: => EsoParser[IndexedSeqOps[A, SS, S]]): EsoParser[SS[B]] = prod(p, q) map {case (e, v) => e +: v}
  def concat[A, SS[_] <: Iterable[_], S, B >: A, T <: IterableOnce[B]](p: => EsoParser[IterableOps[A, SS, S]], q: => EsoParser[T]): EsoParser[SS[B]] = prod(p, q) map {case (a, b) => a ++ b}
  def concatString(p: => EsoParser[String], q: => EsoParser[String]): EsoParser[String] = prod(p, q) map {case (a, b) => a + b}
  
  def longest[A](p: => EsoParser[A], q: => EsoParser[A]): EsoParser[A] = sortAlt(p, q){case ((_, _, ps, pe), (_, _, qs, qe)) => (pe - ps) >= (qe - qs)}
  def earliest[A](p: => EsoParser[A], q: => EsoParser[A]): EsoParser[A] = sortAlt(p, q){case ((_, _, ps, _), (_, _, qs, _)) => ps <= qs}
  
  def all[A](parser: => EsoParser[A], num: Int = 0): EsoParser[Vector[A]] = {
    lazy val p = parser
    lazy val base = EsoParser.empty(Vector[A]())
    lazy val init: EsoParser[Vector[A]] = Vector.fill(num)(p).foldLeft(base){case (v, e) => append(v, e)}
    lazy val s: EsoParser[Vector[A]] = alt(prepend(p, s), base)
    concat(init, s)}
  
  def limp[A, B](p: => EsoParser[A], q: => EsoParser[B]): EsoParser[A] = prod(p, q) map {case (a, _) => a}
  def rimp[A, B](p: => EsoParser[A], q: => EsoParser[B]): EsoParser[B] = prod(p, q) map {case (_, b) => b}
  
  def const[A, B](p: => EsoParser[A], v: => B): EsoParser[B] = p map (_ => v)
}