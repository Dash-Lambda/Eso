package emmental

import common_test.EsoSpec

class EmmentalSpec extends EsoSpec{
  testAgainstOutput(Emmental, first=true)("hworld.emm", "#0#10#33#100#108#114#111#119#32#44#111#108#108#101#72...............", "", s"Hello, world!\n${0.toChar}")
}