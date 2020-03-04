package unlambda

import common_test.EsoSpec

class LambdaToUnlambdaSpace extends EsoSpec{
  testAllTranspiledAgainstProgramResult(Unlambda, LambdaToUnlambda)(
    ("absCountUnlambda.txt", "", "1\n22\n333"))
}
