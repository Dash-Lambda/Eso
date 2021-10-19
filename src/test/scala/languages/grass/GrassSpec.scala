package languages.grass

import common_test.EsoSpec

class GrassSpec extends EsoSpec{
  testAgainstOutput(Grass, first=true)("hworld.grs", grabFile("hworld.grs"), "", "Hello, world!")
  testRTWithFile(Grass)("hworld.grs")
}
