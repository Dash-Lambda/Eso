package glypho

import common_test.EsoSpec

import scala.util.{Failure, Success}

class GlyphoShorthandSpec extends EsoSpec{
  val hworldg: String = grabFile("hworld.glo")
  val hworlds: String = grabFile("hworld.glos")
  
  "GlyphoShorthand" should "preserve the behavior of hworld.glos" in {
    val progTry = GlyphoShorthand(defaultConfig)(hworlds)
    progTry match{
      case Success(prog) =>
        val res = getOutputString(Glypho, prog)
        assertResult(Success("Hello, World!"))(res)
      case Failure(e) => fail(s"Translation Failed ($e)")}}
  
  it should "preserve the behavior of hworld.glo" in {
    val progTry1 = GlyphoShorthand.unapply(defaultConfig)(hworldg)
    val progTry2 = progTry1.flatMap(GlyphoShorthand(defaultConfig))
    progTry2 match{
      case Success(prog) =>
        val res = getOutputString(Glypho, prog)
        assertResult(Success("Hello, World!"))(res)
      case Failure(e) => fail(s"Translation Failed ($e)")}}
}
