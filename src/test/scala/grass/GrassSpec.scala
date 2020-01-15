package grass

import common_test.EsoSpec

class GrassSpec extends EsoSpec{
  val hworld: String =
    """|wWwwwWwwwwwWWWWwv
       |wwWWWwWWWwwwv
       |Wwv
       |wwWWwWWWwv
       |Wwv
       |wwwWWWwwWwwWWWWwv
       |WwwwWWWWwWWWWwwwWwwWWWWWwWWWWWWwv
       |wwWWwWWWwv
       |WwwWWwwwwWWWwwwwwwWWWWWWWWWWwwwwwwwwWwwWWwwwwWWWwwwwwwv
       |wwwWWWwwWwwWWWWwv
       |WwwwwwwWWwwwwWWWwwwwWWWWwWWWWWWWWWWWWWWWWWWWwwwwwWwwWWwWWWwWWWWwv
       |wWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwwwwwwwwWwwwwwwwwwWwwwwwwWwwwwwwwWwwwwwwwWwwwwwwwwwwwwwwwwwwwWwwwwwwwwwwwwwwwwwwwwwwv
       |wwv
       |WWwWwwWwwwwwwWwwwwwwWwwwwwwwwwWwwwwwwwwwwwwwwwwwwWwwwwwwwwwwwwwwwww""".stripMargin
  val hworldRes: String = "Hello, world!"
  
  "Grass" should "run hworld.grs correctly" in assert(testInterp(Grass, defaultConfig, hworld)(outputEquals(hworldRes)))
}
