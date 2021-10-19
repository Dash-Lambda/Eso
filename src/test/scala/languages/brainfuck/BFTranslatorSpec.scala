package languages.brainfuck

import common_test.EsoSpec

abstract class BFTranslatorSpec extends EsoSpec{
  val hworldb: String = grabFile("hworld.b")
}

class FlufflePuffSpec extends BFTranslatorSpec{
  testAllTranslatedAgainstOutput(FlufflePuff)(
    ("hworld.fl", grabFile("hworld.b"), false),
    ("hworld.b", grabFile("hworld.fl"), true))
}

class OokSpec extends BFTranslatorSpec{
  testAllTranslatedAgainstOutput(Ook)(
    ("hworld.ook", grabFile("hworld.b"), false),
    ("hworld.b", grabFile("hworld.ook"), true))
}