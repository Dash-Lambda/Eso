package glypho

import common_test.EsoSpec

class GlyphoSpec extends EsoSpec{
  testAgainstOutput(Glypho, first=true)("hworld.glo", grabFile("hworld.glo"), "", "Hello, World!")
}
