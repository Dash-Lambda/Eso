package metatape

import common_test.EsoSpec

class MetatapeSpec extends EsoSpec{
  testAllAgainstOutput(Metatape)(
    ("hworld.mt", "", "Hello world!"),
    ("bitWidth.mt", "", "Hello World! 255\n"))
  testRTWithAllFiles(Metatape)(
    ("hworld.mt", ""),
    ("bitWidth.mt", ""))
}
