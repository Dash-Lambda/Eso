package path

import common_test.EsoSpec

import scala.util.{Success, Try}

class PATHSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.path")
  val res: Try[String] = getOutputString(PATH, hworld)
  
  "PATH" should "run hworld.path correctly" in assertResult(Success("Hello world!"))(res)
}
