package whitespace

import common_test.EsoSpec

class WhiteSpaceSpec extends EsoSpec{
  testAllAgainstOutput(WhiteSpace)(
    ("hworld.ws", "", "Hello, world!"))
  testRTWithFile(WhiteSpace)("hworld.ws")
}