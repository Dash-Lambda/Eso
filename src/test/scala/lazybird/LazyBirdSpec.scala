package lazybird

import common_test.EsoSpec

class LazyBirdSpec extends EsoSpec{
  def fibs: LazyList[Int] = LazyList.iterate((0, 1)){case (a, b) => (b, a + b)}.map(_._1)
  def fibStr(num: Int): String = fibs.take(num).map(n => "1"*n).mkString("\n")
  testAllAgainstOutputAutoLimit(LazyBird)(
    ("hworld.lzb", "", "hello, world!", false),
    ("fib.lzb", "", fibStr(20), true),
    ("fibComb.lzb", "", fibStr(20), true),
    ("asciiCat.lzb", ('a' to 'z').mkString, ('a' to 'z').map(c => "*"*c.toInt).mkString("\n"), true),
    ("equals.lzb", "", "10", false))
  
  it should "interpret named combinators correctly" in {
    def parsedEquals(a: String, b: String): Unit = assertResult(LazyBird.lzbParser.parseOne(a))(LazyBird.lzbParser.parseOne(b))
    val pairs = Vector(
      "m" -> "``sii",
      "0" -> "`ki",
      "w" -> "``ss`ki",
      "u" -> "``s`k`si``sii",
      "o" -> "`si",
      "t" -> "``s`k`sik",
      "l" -> "``s``s`ksk`k``sii",
      "b" -> "``s`ksk",
      "c" -> "``s``s`k``s`ksks`kk",
      "q" -> "``s`k`s``s`kskk",
      "v" -> "``s`k``s``s`k``s`ksks`kk``s`k`sik",
      "@" -> "``s``si`ks`kk")
    for((a, b) <- pairs) parsedEquals(a, b)}
}
