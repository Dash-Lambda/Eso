package slashes

import common_test.EsoSpec

class SlashesSpec extends EsoSpec{
  testAllAgainstOutput(Slashes)(
    ("hworld.slash", "", "Hello, world!"))
  testRTWithFile(Slashes)("hworld.slash")
}
