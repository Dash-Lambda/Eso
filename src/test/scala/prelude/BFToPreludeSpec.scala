package prelude

import common_test.EsoSpec

import scala.util.Success

class BFToPreludeSpec extends EsoSpec{
  val hworldb: String = grabFile("hworld.b")
  
  "BFToPrelude" should "preserve the behavior of hworld.b" in{
    val tProg = BFToPrelude(defaultConfig)(hworldb)
    val res = tProg flatMap (p => getOutputString(Prelude, p))
    assertResult(Success("Hello World!\n"))(res)}
}
