package lazyk

import common_test.EsoSpec

class LazyKAnyToUnlSpec extends EsoSpec{
  testAllTranslatedAgainstProgramResult(LazyK, LazyKAnyToUnl, canBeSame=true)(
    ("hworldCC.lazy", "", "Hello, world!\n", false),
    ("hworldIota.lazy", "", "Hello, world!\n", false),
    ("hworldJot.lazy", "", "Hello, world!\n", false),
    ("hworld.lazy", "", "Hello, world!\n", true))
}
