package thue

import common_test.EsoSpec

class ThueSpec extends EsoSpec{
  testAllAgainstOutput(Thue)(
    ("hworld.th", "", "Hello World!"))
}
