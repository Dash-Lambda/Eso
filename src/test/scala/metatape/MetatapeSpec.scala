package metatape

import common_test.EsoSpec

class MetatapeSpec extends EsoSpec{
  val hworld: String =
    """|ex>
       |// The tape head is now pointing to a null cell, with a non-null cell to the
       |// left.
       |!H !e !l !l !o !_ !w !o !r !l !d !!
       |
       |// Each of these functions moves left for a 0 bit and right for a 1 bit to
       |// output the ASCII value for the given character.
       |@ H { o<o>oo<o>ooo }
       |@ e { o<oo>oo<o>o<o> }
       |@ l { o<oo>o<oo>oo }
       |@ o { o<oo>o<oooo> }
       |@ _ { oo<o>ooooo }
       |@ w { o<ooo>o<ooo> }
       |@ r { o<ooo>oo<o>o }
       |@ d { o<oo>oo<o>oo }
       |@ ! { oo<o>oooo<o> }""".stripMargin
  val hworldb: String =
    """|@r{[[(e(|x<])|<])x}
       |@+{[(n>]|ex)!r}
       |@m{>>>>>>>>>ex<<<<<<<<}
       |@-{[(n|ex>])!r}
       |@,{eeexx>>>>>>>>exi<exi<exi<exi<exi<exi<exi<exi<x}
       |@.{e>>>>>>>>o<o<o<o<o<o<o<o<x}
       |@f{!re>ex<x}
       |@c{eeex>n<x >(!f|>(!f|>(!f|>(!f|>(!f|>(!f|>(!f|>(!f|!r))))))))x}
       |
       |eeexx>>!+>>>>!+x!cee>(<xx[>eeexx>!+>>!+>>>!+x>eeexx>>!+>>>>!+x>eeexx>!+>>!+x>eeexx>!+x<<<<eeexx!m!-x!cee>(<xx]|<xx)|<xx)>eeexx>>!+x!.>eeexx>!+x!.eeexx>!+>>!+>>>!+x!.!.eeexx>!+>>!+x!.>eeexx>>!+x!.<<eeexx>!+>>!+>>>!+>>>>!+x!.>!.eeexx>!+>>!+x!.eeexx!m>!-!m>>!-x!.eeexx!m>>>!-x!.>eeexx>!+x!.>!.""".stripMargin
  val hworldRes: String = "Hello world!"
  val hworldbRes: String = "Hello World!\n"
  
  "Metatape" should "run hworld.mt correctly" in assert(testInterp(Metatape, defaultConfig, hworld)(outputEquals(hworldRes)))
  it should "run transpiled hworld.b correctly" in assert(testInterp(Metatape, defaultConfig, hworldb)(outputEquals(hworldbRes)))
}
