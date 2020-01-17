package snusp

import common_test.EsoSpec

import scala.util.Success

class BFToSNUSPSpec extends EsoSpec{
  val bitWidthb: String = grabFile("bitWidth.b")
  
  "BFToSNUSP" should "preserve the behavior of bitWidth.b" in {
    BFToSNUSP(defaultConfig)(bitWidthb) match{
      case Success(bwsnu) =>
        val res = getOutputString(SNUSP, bwsnu)
        assertResult(Success("Hello, world!\n"))(res)
      case f => fail(s"Returned $f")}}
}
