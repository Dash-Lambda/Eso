package snusp

import common_test.EsoSpec

import scala.util.{Success, Try}

class SNUSPSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.snusp")
  val res: Try[String] = getOutputString(SNUSP, hworld)
  
  "SNUSP" should "run hworld.snusp correctly" in assertResult(Success("Hello World!\n"))(res)
}
