package snusp

import common_test.EsoSpec

import scala.util.{Success, Try}

class SNUSPSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.snusp")
  val bitWidth: String = grabFile("bitWidth.snusp")
  lazy val res1: Try[String] = getOutputString(SNUSP, hworld)
  lazy val res2: Try[String] = getOutputString(SNUSP, bitWidth)
  
  "SNUSP" should "run hworld.snusp correctly" in assertResult(Success("Hello World!\n"))(res1)
  it should "run bitWidth.snusp correctly" in assertResult(Success("Hello, world!\n"))(res2)
}
