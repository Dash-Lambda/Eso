package thue

import common_test.EsoSpec

import scala.util.{Success, Try}

class ThueSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.th")
  val res: Try[String] = getOutputString(Thue, hworld)
  
  "Thue" should "run hworld.th correctly" in assertResult(Success("Hello World!"))(res)
}
