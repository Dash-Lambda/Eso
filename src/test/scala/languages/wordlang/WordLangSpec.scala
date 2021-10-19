package languages.wordlang

import common_test.EsoSpec

class WordLangSpec extends EsoSpec{
  testAllAgainstOutput(WordLang)(
    ("helloo.wl", "", ";"),
    ("camo.wl", "", "Hlt|"),
    ("listing.wl", "", (100 to 0 by -1).map(_.toChar).mkString),
    ("flag.wl", "", "flag{3100µ¢4173µÎ3575µú2977µ'2379µS1781µ\u007F2052}"),
    ("letsLive.wl", "", "LÄ;Ù"),
    ("someday.wl", "", "Ö3"),
    ("listChars.wl", "", (100 to 0 by -1).map(_.toChar).mkString),
    ("cat.wl", "a", "I=\nI=a"))
  testRTWithAllFiles(WordLang)(
    ("helloo.wl", ""),
    ("camo.wl", ""),
    ("listing.wl", ""),
    ("flag.wl", ""),
    ("letsLive.wl", ""),
    ("someday.wl", ""),
    ("listChars.wl", ""),
    ("cat.wl", "a"))
}
