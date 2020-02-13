package common

import spire.math.SafeLong
import spire.math.Integral
import spire.implicits._

import scala.annotation.tailrec

object PrimeNumTools {
  def primesLazy: LazyList[Int] = {
    lazy val ps: LazyList[Int] = 2 #:: 3 #:: LazyList.from(5, 2).filter(n => !ps.takeWhile(p => p*p <= n).exists(n%_ == 0))
    ps}
  def primesLazySL: LazyList[SafeLong] = {
    def slFrom(n0: SafeLong, stp: SafeLong = 1): LazyList[SafeLong] = LazyList.iterate(n0)(n => n + stp)
    lazy val ps: LazyList[SafeLong] = SafeLong(2) #:: SafeLong(3) #:: slFrom(5, 2).filter(n => !ps.takeWhile(p => p*p <= n).exists(n%_ == 0))
    ps}
  
  //This is not really mine, it was taken from the excellent Scala entry in the Sieve of Eratosthenes task https://rosettacode.org/wiki/Sieve_of_Eratosthenes#Scala
  def birdPrimes: Iterator[SafeLong] = {
    case class CIS[A](head: A, next: () => CIS[A])
    def merge(xs: CIS[SafeLong], ys: CIS[SafeLong]): CIS[SafeLong] = {
      val (x, y) = (xs.head, ys.head)
      if(y > x) CIS(x, () => merge(xs.next(), ys))
      else if(x > y) CIS(y, () => merge(xs, ys.next()))
      else CIS(x, () => merge(xs.next(), ys.next()))}
    def pMults(p: SafeLong): CIS[SafeLong] = {
      def nxtCull(cull: SafeLong): CIS[SafeLong] = CIS[SafeLong](cull, () => nxtCull(cull + 2*p))
      nxtCull(p*p)}
    def allMults(ps: CIS[SafeLong]): CIS[CIS[SafeLong]] = CIS[CIS[SafeLong]](pMults(ps.head), () => allMults(ps.next()))
    def join(ams: CIS[CIS[SafeLong]]): CIS[SafeLong] = CIS[SafeLong](ams.head.head, () => merge(ams.head.next(), join(ams.next())))
    def oddPrimes: CIS[SafeLong] = {
      def oddPrms(n: Int, comps: CIS[SafeLong]): CIS[SafeLong] = {
        if(n >= comps.head) oddPrms(n + 2, comps.next())
        else CIS[SafeLong](n, () => oddPrms(n + 2, comps))}
      CIS(3, () => oddPrms(5, join(allMults(oddPrimes))))}
    Iterator.single(SafeLong(2)) ++ Iterator.unfold(oddPrimes: CIS[SafeLong]){case CIS(n, f) => Some((n, f()))}}
  
  def isPrime[I: Integral](n: I): Boolean = {
    @tailrec
    def pdo(f: I): Boolean = {
      if(n.emod(f) == 0) false
      else if(f*f >= n) true
      else pdo(f + 2)}
    n == 2 || (n.emod(2) == 0 && pdo(3))}
  
  def factor(num: SafeLong): Vector[Int] = {
    @tailrec
    def fdo(init: SafeLong, f: SafeLong, c: Int = 0): (SafeLong, Int) = {
      if (init % f == 0) fdo(init / f, f, c + 1)
      else (init, c)}
    @tailrec
    def fgo(n: SafeLong, ac: Vector[Int] = Vector[Int](), src: LazyList[SafeLong] = birdPrimes.to(LazyList)): Vector[Int] = fdo(n, src.head) match {
      case (nxt, 0) if nxt == 1 => ac
      case (nxt, p) =>
        if (nxt == 1) ac :+ p
        else fgo(nxt, ac :+ p, src.tail)}
    fgo(num.abs)}
}
