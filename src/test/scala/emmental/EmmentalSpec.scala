package emmental

import common_test.EsoSpec

import scala.util.{Success, Try}

class EmmentalSpec extends EsoSpec{
  val hworld: String = "#0#10#33#100#108#114#111#119#32#44#111#108#108#101#72..............."
  lazy val res: Try[String] = getOutputString(Emmental, hworld)
  "Emmental" should "run hworld.emm correctly" in assertResult(Success(s"Hello, world!\n${0.toChar}"))(res)
}