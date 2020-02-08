package lazyk

import common_test.EsoSpec

class LazyKSpec extends EsoSpec{
  testAllAgainstOutputWithLimit(LazyK)(
    ("hworld.lazy", "", "Hello, world!\n", -1),
    ("unlambda.lazy", grabFile("hworld.unl"), "Hello world\n", -1),
    ("primes.lazy", "", "2\n3\n5\n7\n11\n13\n17", 16))
}
