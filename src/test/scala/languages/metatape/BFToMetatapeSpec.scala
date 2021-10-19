package languages.metatape

import common_test.EsoSpec

import scala.util.Success

class BFToMetatapeSpec extends EsoSpec{
  val bitWidthb: String = grabFile("bitWidth.b")
  
  "BFToMetatape" should "preserve the behavior of bitWidth.b" in {
    BFToMetatape(defaultConfig)(bitWidthb) match{
      case Success(bwmt) =>
        val res = getOutputString(Metatape, bwmt)
        assertResult(Success("Hello World! 255\n"))(res)
      case f => fail(s"Returned $f")}}
}
