package languages.deadfish

import common_test.EsoSpec

class DeadfishSpec extends EsoSpec{
  testAgainstOutput(Deadfish, first=true)("hworld.df", grabFile("hworld.df"), "", "Hello world")
  testRTWithAllFiles(Deadfish)(("hworld.df", ""))
}
