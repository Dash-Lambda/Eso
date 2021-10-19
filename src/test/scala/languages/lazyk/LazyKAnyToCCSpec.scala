package languages.lazyk

import common_test.EsoSpec

class LazyKAnyToCCSpec extends EsoSpec{
  testAllTranslatedAgainstProgramResult(LazyK, LazyKAnyToCC, canBeSame=true)(
    ("hworldCC.lazy", "", "Hello, world!\n", true),
    ("hworldIota.lazy", "", "Hello, world!\n", false),
    ("hworldJot.lazy", "", "Hello, world!\n", false),
    ("hworld.lazy", "", "Hello, world!\n", false))
}
