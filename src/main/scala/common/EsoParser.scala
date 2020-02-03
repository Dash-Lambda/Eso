package common

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

abstract class EsoParser[A, B]{
  def apply(inp: A): Option[B]
  def step(inp: A): Option[(B, A)]
  
  def parseAll(inp: A): Vector[B] = Vector.unfold(inp)(step)
  def parseAllLazy(inp: A): LazyList[B] = LazyList.unfold(inp)(step)
  
  def map[C](f: B => C): EsoParser[A, C] = {
    ChunkParser[A, C]{inp =>
      step(inp) map {
        case (r, s) => (f(r), s)}}}
  
  def ~[C](p: EsoParser[B, C]): EsoParser[A, C] = {
    ChunkParser[A, C]{inp =>
      step(inp) flatMap{
        case (lnk, ops) => p(lnk) map (r => (r, ops))}}}
  
  def <+>(p: EsoParser[A, B]): OrElseParser[A, B] = p match{
    case OrElseParser(ps) => OrElseParser(this +: ps)
    case _ => OrElseParser(Vector(this, p))}
}

case class OrElseParser[A, B](parsers: Vector[EsoParser[A, B]]) extends EsoParser[A, B]{
  def apply(inp: A): Option[B] = {
    parsers
      .iterator
      .map(p => p(inp))
      .collectFirst{case Some(a) => a}}
  def step(inp: A): Option[(B, A)] = {
    parsers
      .iterator
      .map(p => p.step(inp))
      .collectFirst{case Some(s) => s}}
  
  override def <+>(p: EsoParser[A, B]): OrElseParser[A, B] = p match{
    case OrElseParser(ps) => OrElseParser(parsers :++ ps)
    case _ => OrElseParser(parsers :+ p)}
}

case class ChunkParser[A, B](f: A => Option[(B, A)]) extends EsoParser[A, B]{
  def apply(inp: A): Option[B] = f(inp) map (_._1)
  def step(inp: A): Option[(B, A)] = f(inp)
}

case class PartialParser[A, B](f: PartialFunction[A, (B, A)]) extends EsoParser[A, B]{
  def apply(inp: A): Option[B] = f.lift(inp).map(_._1)
  def step(inp: A): Option[(B, A)] = f.lift(inp)
}

case class RecurParser[A, B](depth: Int)(recur: A => Option[A])(collect: Seq[B] => B)(endParser: EsoParser[A, B]) extends EsoParser[A, B]{
  def apply(inp: A): Option[B] = step(inp) map (_._1)
  def step(inp: A): Option[(B, A)] = {
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
    def pdo(src: A, cc: Cont): Option[(B, A)] = {
      recur(src) match{
        case Some(nxt) => pdo(nxt, RecCont(depth, Vector(), cc))
        case None => endParser.step(src) match{
          case Some((p, nxt)) => pdo(nxt, cc(p))
          case None => cc match{
            case ResCont(res) => Some((res, src))
            case _ => None}}}}
    pdo(inp, FinCont)}
}

case class RegexParser[B](reg: Regex)(f: Match => B) extends EsoParser[String, B]{
  def apply(inp: String): Option[B] = reg.findFirstMatchIn(inp) map f
  def step(inp: String): Option[(B, String)] = reg.findFirstMatchIn(inp) map (m => (f(m), m.after.toString))
}
object RegexParser{
  def apply[B](reg: String)(f: Match => B): RegexParser[B] = RegexParser(reg.r)(f)
}