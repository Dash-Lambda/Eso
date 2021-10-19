package languages.wierd

import common_test.EsoSpec

class WierdSpec extends EsoSpec{
  testAllAgainstOutput(Wierd)(
    ("quine.wd", "", grabFile("quine.wd")))
  testRTWithFile(Wierd)("quine.wd")
}
