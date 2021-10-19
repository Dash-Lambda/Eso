package languages.lazyk

import common_test.EsoSpec

class LazyKAnyToIotaSpec extends EsoSpec{
  testAllTranslatedAgainstProgramResult(LazyK, LazyKAnyToIota, canBeSame=true)(
    ("hworldCC.lazy", "", "Hello, world!\n", false),
    ("hworldIota.lazy", "", "Hello, world!\n", true),
    ("hworldJot.lazy", "", "Hello, world!\n", false),
    ("hworld.lazy", "", "Hello, world!\n", false))
}
