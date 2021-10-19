package languages.alpl

import common_test.EsoSpec

class ALPLSpec extends EsoSpec{
  testAllAgainstOutputAutoLimit(ALPL)(
    ("hworld.alpl", "", "Hello World!", false),
    ("cat.alpl", "testing 123", "testing 123", true))
  testRTWithAllFilesLimited(ALPL)(
    ("hworld.alpl", "", -1),
    ("cat.alpl", "testing 123", 11))
}
