package path

import common_test.EsoSpec

class PATHSpec extends EsoSpec{
  testAllAgainstOutput(PATH)(
    ("hworld.path", "", "Hello world!"))
  testRTWithFile(PATH)("hworld.path")
}
