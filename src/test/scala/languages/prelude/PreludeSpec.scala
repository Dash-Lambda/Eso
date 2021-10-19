package languages.prelude

import common_test.EsoSpec

import scala.util.Success

class PreludeSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.pld")
  
  "Prelude" should "run hworld.pld correctly in parallel" in {
    val res = getOutputString(Prelude, hworld, Seq(), defaultConfig.set("preludePar", b=true))
    assertResult(Success("Hello world!"))(res)}
  
  it should "run hworld.pld correctly in sequence" in {
    val res = getOutputString(Prelude, hworld, Seq(), defaultConfig.set("preludePar", b=false))
    assertResult(Success("Hello world!"))(res)}
  
  it should "return a referentially transparent function in parallel" in testRTWithSource(Prelude, config=defaultConfig.set("preludePar", b=true))(hworld)
  it should "return a referentially transparent function in sequence" in testRTWithSource(Prelude, config=defaultConfig.set("preludePar", b=false))(hworld)
}
