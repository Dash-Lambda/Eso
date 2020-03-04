package lazyk

import common_test.EsoSpec

class LambdaToLazyKUnlSpec extends EsoSpec{
  testAllTranspiledAgainstProgramResult(LazyK, LambdaToLazyKUnl)(
    ("absSpaceLazyK.txt", "", " "))
}
