package volatile

import common_test.EsoSpec

import scala.util.Success

class VolatileSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.vol")
  
  "Volatile" should "run hworld.vol correctly" in{
    val res = getOutputString(Volatile, hworld)
    assertResult(Success("Hello, world!\n"))(res)}
}
