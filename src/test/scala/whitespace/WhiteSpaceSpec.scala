package whitespace

import common_test.EsoSpec

import scala.util.{Success, Try}

class WhiteSpaceSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.ws")
  val res: Try[String] = getOutputString(WhiteSpace, hworld)
  
  "WhiteSpace" should "run hworld.ws correctly" in assertResult(Success("Hello, world!"))(res)
}