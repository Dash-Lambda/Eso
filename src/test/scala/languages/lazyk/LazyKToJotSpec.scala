package languages.lazyk

import common_test.EsoSpec

class LazyKToJotSpec extends EsoSpec{
  testAllTranslatedAgainstProgramResult(LazyK, LazyKAnyToJot, canBeSame=true)(
    ("hworldCC.lazy", "", "Hello, world!\n", false),
    ("hworldIota.lazy", "", "Hello, world!\n", false),
    ("hworldJot.lazy", "", "Hello, world!\n", true),
    ("hworld.lazy", "", "Hello, world!\n", false))
}
