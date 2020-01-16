package deadfish

import common_test.EsoSpec

import scala.util.{Success, Try}

class DeadfishSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.df")
  lazy val res: Try[String] = getOutputString(Deadfish, hworld)
  
  "Deadfish" should "run hworld.df correctly" in assertResult(Success("Hello world"))(res)
}
