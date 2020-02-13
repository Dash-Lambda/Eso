package lazyk

import common_test.EsoSpec

class LazyKUnlToCCSpec extends EsoSpec{
  val hworld: String = filterChars(grabFile("hworld.lazy"), "`ski")
  val hworldcc: String = filterChars(grabFile("hworldCC.lazy"), "(SKI)")
  
  "LazyKUnlToCC" should "correctly translate from Unlambda to SKI CC" in testTranslatorAgainstProgramResult(LazyK, LazyKUnlToCC, rev=false, hworld, "Hello, world!\n")
  it should "correctly translate from SKI CC to Unlambda" in testTranslatorAgainstProgramResult(LazyK, LazyKUnlToCC, rev=true, hworldcc, "Hello, world!\n")
}
