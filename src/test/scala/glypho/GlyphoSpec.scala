package glypho

import common_test.EsoSpec

import scala.util.Success

class GlyphoSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.glo")
  
  "Glypho" should "run hworld.glo correctly" in {
    val res = getOutputString(Glypho, hworld)
    assertResult(Success("Hello, World!"))(res)}
}
