package metatape

import common_test.EsoSpec

import scala.util.{Success, Try}

class MetatapeSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.mt")
  val bitwid: String = grabFile("bitWidth.mt")
  lazy val res1: Try[String] = getOutputString(Metatape, hworld)
  lazy val res2: Try[String] = getOutputString(Metatape, bitwid)
  
  "Metatape" should "run hworld.mt correctly" in assertResult(Success("Hello world!"))(res1)
  it should "run transpiled bitWidth.mt correctly" in assertResult(Success("Hello World! 255\n"))(res2)
}
