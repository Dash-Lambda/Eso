package prelude

import common_test.EsoSpec

import scala.util.Success

class PreludeSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.pld")
  val hworldb: String = grabFile("hworld.b")
  
  "Prelude" should "run hworld.pld correctly" in {
    val res = getOutputString(Prelude, hworld)
    assertResult(Success("Hello world!"))(res)}
  
  "BFToPrelude" should "preserve the behavior of hworld.b" in{
    val tProg = BFToPrelude(defaultConfig)(hworldb)
    val res = tProg flatMap (p => getOutputString(Prelude, p))
    assertResult(Success("Hello World!\n"))(res)}
}
