package languages.volatile

import common_test.EsoSpec

class VolatileSpec extends EsoSpec{
  testAllAgainstOutput(Volatile)(
    ("hworld.vol", "", "Hello, world!\n"))
  testRTWithFile(Volatile)("hworld.vol")
}
