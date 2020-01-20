package null_lang

import common_test.EsoSpec

import scala.util.Success

class NULLSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.nul")
  
  "NULL" should "run hworld.nul correctly" in {
    val res = getOutputString(NULL, hworld)
    assertResult(Success("Hello, world!\n"))(res)}
}
