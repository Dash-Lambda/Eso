package lazyk

import common.PrimeNumTools
import common_test.EsoSpec

class LazyKSpec extends EsoSpec{
  testAllAgainstOutputAutoLimit(LazyK)(
    ("hworld.lazy", "", "Hello, world!\n", false),
    ("unlambda.lazy", grabFile("hworld.unl"), "Hello world\n", false),
    ("primes.lazy", "", PrimeNumTools.primesLazy.take(100).mkString("\n"), true),
    ("fib.lazy", "", Vector(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55).map("*"*_).mkString("\n"), true),
    ("reverse.lazy", "abc", "cba", false))
}
