package lazyk

import common.PrimeNumTools
import common_test.EsoSpec

class LazyKSpec extends EsoSpec{
  testAllAgainstOutputAutoLimit(LazyK)(
    ("hworld.lazy", "", "Hello, world!\n", false),
    ("unlambda.lazy", grabFile("hworld.unl"), "Hello world\n", false),
    ("primes.lazy", "", PrimeNumTools.primesLazy.take(100).mkString("\n"), true),
    ("fib.lazy", "", Vector(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55).map("*"*_).mkString("\n"), true),
    ("reverse.lazy", "abc", "cba", false),
    ("cat.lazy", "test", "test", false))
  testRTWithAllFilesLimited(LazyK)(
    ("hworld.lazy", "", -1),
    ("unlambda.lazy", grabFile("hworld.unl"), -1),
    ("primes.lazy", "", 100),
    ("fib.lazy", "", 100),
    ("reverse.lazy", "testing", -1),
    ("cat.lazy", "input test", -1))
}
