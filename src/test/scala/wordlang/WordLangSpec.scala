package wordlang

import common_test.EsoSpec

import scala.util.Success

class WordLangSpec extends EsoSpec{
  val helloo: String = grabFile("helloo.wl")
  val camo: String = grabFile("camo.wl")
  val listing: String = grabFile("listing.wl")
  val flag: String = grabFile("flag.wl")
  
  val hres: String = ";"
  val cres: String = "Hlt|"
  val lres: String = (100 to 0 by -1).map(_.toChar).mkString
  val fres: String = "flag{3100µ¢4173µÎ3575µú2977µ'2379µS1781µ\u007F2052}"
  
  val pairs: Vector[(String, String, String)] = Vector(
    ("helloo.wl", helloo, hres),
    ("camo.wl", camo, cres),
    ("listing.wl", listing, lres),
    ("flag.wl", flag, fres))
  
  pairs.head match{
    case (nam, p, t) =>
      "WordLang" should s"run $nam correctly" in {
        val res = getOutputString(WordLang, p)
        assertResult(Success(t))(res)}}
  for((nam, p, t) <- pairs.tail){
    it should s"run $nam correctly" in {
      val res = getOutputString(WordLang, p)
      assertResult(Success(t))(res)}}
}
