package prelude

import common_test.EsoSpec

import scala.util.Success

class PreludeSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.pld")
  val hworldb: String = grabFile("hworld.b")
  
  "Prelude" should "run hworld.pld correctly in parallel" in {
    val res = getOutputString(Prelude, hworld, Seq(), defaultConfig.set("preludePar", b=true))
    assertResult(Success("Hello world!"))(res)}
  
  it should "run hworld.pld correctly in sequence" in {
    val res = getOutputString(Prelude, hworld, Seq(), defaultConfig.set("preludePar", b=false))
    assertResult(Success("Hello world!"))(res)}
  
  "BFToPrelude" should "preserve the behavior of hworld.b" in{
    val tProg = BFToPrelude(defaultConfig)(hworldb)
    val res = tProg flatMap (p => getOutputString(Prelude, p))
    assertResult(Success("Hello World!\n"))(res)}
}
