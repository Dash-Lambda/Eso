package brainfuck

import common_test.EsoSpec

import scala.util.Success

abstract class BFTranslatorSpec extends EsoSpec{
  val hworldb: String = grabFile("hworld.b")
}

class FlufflePuffSpec extends BFTranslatorSpec{
  val hworldfl: String = grabFile("hworld.fl")
  
  "FlufflePuff" should "translate from FlufflePuff to BrainFuck correctly" in {
    val trans = FlufflePuff(defaultConfig)(hworldfl)
    assertResult(Success(hworldb))(trans)}
  
  it should "translate from BrainFuck to FlufflePuff correctly" in {
    val trans = FlufflePuff.unapply(defaultConfig)(hworldb)
    assertResult(Success(hworldfl))(trans)}
}

class OokSpec extends BFTranslatorSpec{
  val hworldook: String = grabFile("hworld.ook")
  
  "Ook" should "translate from Ook to BrainFuck correctly" in {
    val trans = Ook(defaultConfig)(hworldook)
    assertResult(Success(hworldb))(trans)}
  
  it should "translate from BrainFuck to Ook correctly" in {
    val trans = Ook.unapply(defaultConfig)(hworldb)
    assertResult(Success(hworldook))(trans)}
}