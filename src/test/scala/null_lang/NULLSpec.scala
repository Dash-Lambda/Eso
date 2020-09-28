package null_lang

import common_test.EsoSpec

class NULLSpec extends EsoSpec{
  testAllAgainstOutput(NULL)(
    ("hworld.nul", "", "Hello, world!\n"))
  testRTWithFile(NULL)("hworld.nul")
}
