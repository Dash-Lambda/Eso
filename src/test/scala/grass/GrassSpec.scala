package grass

import common_test.EsoSpec

import scala.util.{Success, Try}

class GrassSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.grs")
  lazy val res: Try[String] = getOutputString(Grass, hworld)
  
  "Grass" should "run hworld.grs correctly" in assertResult(Success("Hello, world!"))(res)
}
