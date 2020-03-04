package common

import common_test.EsoSpec
import spire.math.SafeLong
import spire.implicits._

class PrimeNumToolsSpec extends EsoSpec{
  val primeRef: Vector[Int] = Vector(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541)
  val primeRefSL: Vector[SafeLong] = primeRef.map(SafeLong(_))
  
  "PrimeNumTools" should "generate a list of prime number Ints by trial division" in {
    val res = PrimeNumTools.primesLazy.take(100).toVector
    assertResult(primeRef)(res)}
  
  it should "generate a list of prime number SafeLongs by trial division" in {
    val res = PrimeNumTools.primesLazySL.take(100).toVector
    assertResult(primeRefSL)(res)}
  
  it should "generate a list of prime number SafeLongs by incremental SOE" in {
    val res = PrimeNumTools.birdPrimes.take(100).toVector
    assertResult(primeRefSL)(res)}
  
  it should "correctly identify primes numbers" in {
    val ps = LazyList.range(1, 100).filter(PrimeNumTools.isPrime)
    val ref = PrimeNumTools.primesLazy.takeWhile(_ < 100)
    assertResult(ref)(ps)}
  
  it should "factor a number such that it is equal to the product of its factors" in {
    val primes = PrimeNumTools.primesLazy
    val factored = LazyList.range(1, 100).map(num => (num, PrimeNumTools.factor(num)))
    def toNum(facs: Vector[Int]): Int = facs.zip(primes).map{case (p, b) => b**p}.product
    for((num, facs) <- factored){
      val res = toNum(facs)
      if(res != num) fail(s"${facs.mkString("[", ", ", "]")} = $res != $num")}}
}
