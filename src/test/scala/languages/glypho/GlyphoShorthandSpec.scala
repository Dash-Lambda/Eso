package languages.glypho

import common_test.EsoSpec

import scala.util.{Failure, Success}

class GlyphoShorthandSpec extends EsoSpec{
  testAllTranslatedAgainstProgramResult(Glypho, GlyphoShorthand)(
    ("hworld.glos", "", "Hello, World!", false))
  
  it should "preserve the behavior of hworld.glo" in {
    val progTry1 = GlyphoShorthand.unapply(defaultConfig)(grabFile("hworld.glo"))
    val progTry2 = progTry1.flatMap(GlyphoShorthand(defaultConfig))
    progTry2 match{
      case Success(prog) =>
        val res = getOutputString(Glypho, prog)
        assertResult(Success("Hello, World!"))(res)
      case Failure(e) => fail(s"Translation Failed ($e)")}}
}
