package platts

import common_test.EsoSpec

class PlattsSpec extends EsoSpec{
  testAllAgainstOutput(Platts)(
    ("hworld.plts", "", "Hello World!"))
}
