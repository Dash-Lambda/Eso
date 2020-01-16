package slashes

import common_test.EsoSpec

import scala.util.{Success, Try}

class SlashesSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.slash")
  val res: Try[String] = getOutputString(Slashes, hworld)
  
  "Slashes" should "run hworld.slash correctly" in assertResult(Success("Hello, world!"))(res)
}
