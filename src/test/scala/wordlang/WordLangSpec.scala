package wordlang

import common_test.EsoSpec

class WordLangSpec extends EsoSpec{
  testAllAgainstOutput(WordLang)(
    ("helloo.wl", "", ";"),
    ("camo.wl", "", "Hlt|"),
    ("listing.wl", "", (100 to 0 by -1).map(_.toChar).mkString),
    ("flag.wl", "", "flag{3100µ¢4173µÎ3575µú2977µ'2379µS1781µ\u007F2052}"))
}
