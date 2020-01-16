package pdoubleprime

import common_test.EsoSpec

import scala.util.{Success, Try}

class PDPSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.pdp")
  val res: Try[String] = getOutputString(PDP, hworld)
  
  "P''" should "run hworld.pdp correctly" in assertResult(Success("Hello world!"))(res)
}
