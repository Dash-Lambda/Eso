package pdoubleprime

import common_test.EsoSpec

class PDPSpec extends EsoSpec{
  testAllAgainstOutput(PDP)(
    ("hworld.pdp", "", "Hello world!"))
}
