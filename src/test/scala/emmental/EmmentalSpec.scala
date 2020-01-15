package emmental

import common_test.EsoSpec

class EmmentalSpec extends EsoSpec{
  val hworld: String = "#0#10#33#100#108#114#111#119#32#44#111#108#108#101#72..............."
  val hworldRes: String = s"Hello, world!\n${0.toChar}"
  "Emmental" should "run hworld.emm correctly" in assert(testInterp(Emmental, defaultConfig, hworld, Seq())(outputEquals(hworldRes)))
}