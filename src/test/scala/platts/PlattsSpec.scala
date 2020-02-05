package platts

import common_test.EsoSpec

import scala.util.Success

class PlattsSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.plts")
  
  "Platts" should "run hworld.plts correctly" in {
    val res = getOutputString(Platts, hworld)
    assertResult(Success("Hello World!"))(res)}
}
