package emmental

import common_test.EsoSpec

class EmmentalSpec extends EsoSpec{
  testAgainstOutput(Emmental, first=true)("hworld.emm", grabFile("hworld.emm"), "", s"Hello, world!\n${0.toChar}")
  testRTWithAllFiles(Emmental)(("hworld.emm", ""))
}