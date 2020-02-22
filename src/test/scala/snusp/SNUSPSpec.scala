package snusp

import common_test.EsoSpec

class SNUSPSpec extends EsoSpec{
  testAllAgainstOutput(SNUSP)(
    ("hworld.snusp", "", "Hello World!\n"),
    ("bitWidth.snusp", "", "Hello, world!\n"))
}
