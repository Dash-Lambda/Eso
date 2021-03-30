package parsers

import common_test.EsoSpec

class EsoParserSpec extends EsoSpec{
  
}

class EsoStringParserSpec extends EsoParserSpec{
  "EsoStringParser" should "parse the first occurrence of its literal" in assertResult(EsoParsed("a", "ba", 0, 1))(EsoStringParser("a")("aba"))
  it should "not parse matches that aren't at the beginning of input" in assertResult(EsoParseFail)(EsoStringParser("a")("bab"))
  it should "match longer literals" in assertResult(EsoParsed("abc", "def", 0, 3))(EsoStringParser("abc")("abcdef"))
}

class EsoRegexParserSpec extends EsoParserSpec{
  "EsoRegexParser" should "return earliest match" in assertResult(EsoParsed("a", "ba", 0, 1))(EsoRegexParser("a".r)("aba"))
  it should "match anywhere in input" in assertResult(EsoParsed("a", "c", 1, 2))(EsoRegexParser("a".r)("bac"))
  it should "return whole match without capture groups" in assertResult(EsoParsed("abbc", "d", 1, 5))(EsoRegexParser("ab*c".r)("aabbcd"))
  it should "return only capture groups if present" in assertResult(EsoParsed("ac", "d", 1, 5))(EsoRegexParser("(a)b*(c)".r)("aabbcd"))
}

class EsoAltParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = EsoRegexParser("a".r)
  val q: EsoParser[String] = EsoRegexParser("b".r)
  
  "EsoAltParser" should "return the first success" in {
    assertResult(EsoParsed("a", "ab", 0, 1))((p | q)("aab"))
    assertResult(EsoParsed("b", "", 2, 3))((q | p)("aab"))
    assertResult(EsoParsed("a", "bb", 0, 1))((p | q)("abb"))
    assertResult(EsoParsed("b", "b", 1, 2))((q | p)("abb"))}
}

class EsoEarliestMatchParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = EsoRegexParser("a".r)
  val q: EsoParser[String] = EsoRegexParser("b".r)
  
  "EsoEarliestMatchParser" should "return the earliest match" in {
    assertResult(EsoParsed("a", "ab", 0, 1))((p || q)("aab"))
    assertResult(EsoParsed("a", "ab", 0, 1))((q || p)("aab"))
    assertResult(EsoParsed("a", "bb", 0, 1))((p || q)("abb"))
    assertResult(EsoParsed("a", "bb", 0, 1))((q || p)("abb"))}
}

class EsoLongestMatchParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = EsoRegexParser("a+".r)
  val q: EsoParser[String] = EsoRegexParser("b+".r)
  
  "EsoLongestMatchParser" should "return the longest match" in {
    assertResult(EsoParsed("aa", "b", 0, 2))((p ||| q)("aab"))
    assertResult(EsoParsed("aa", "b", 0, 2))((q ||| p)("aab"))
    assertResult(EsoParsed("bb", "", 1, 3))((p ||| q)("abb"))
    assertResult(EsoParsed("bb", "", 1, 3))((q ||| p)("abb"))}
}

class EsoFlatMappedParserSpec extends EsoParserSpec{
  val p: EsoParser[Int] = EsoRegexParser("^a*".r) map (_.length)
  def f(n: Int): EsoParser[String] = EsoRegexParser("^.".r) map (_*n)
  
  "EsoFlatMappedParser" should "work the do good right" in {
    assertResult(EsoParsed("bb", "c", 0, 3))((p flatMap f)("aabc"))}
}

class EsoIntoParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = EsoRegexParser(".b".r)
  val q: EsoParser[String] = EsoRegexParser("^.".r)
  
  "EsoIntoParser" should "pass p's output into q" in {
    assertResult(EsoParsed("a", "d", 1, 3))((p >> q) apply "cabd")}
}

class EsoLCondParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = EsoRegexParser("a".r)
  val q: EsoParser[String] = EsoRegexParser("^b".r)
  
  "EsoLCondParser" should "succeed if both succeed" in assertResult(EsoParsed("a", "b", 0, 1))((p <| q)("ab"))
  it should "fail if left fails" in assertResult(EsoParseFail)((p <| q)("cb"))
  it should "fail if right fails" in assertResult(EsoParseFail)((p <| q)("ac"))
  it should "fail if both fail" in assertResult(EsoParseFail)((p <| q)("cc"))
}

class EsoLImpParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = EsoRegexParser("a".r)
  val q: EsoParser[String] = EsoRegexParser("^b".r)
  
  "EsoLImpParser" should "succeed if both succeed" in assertResult(EsoParsed("a", "", 0, 2))((p <& q)("ab"))
  it should "fail if left fails" in assertResult(EsoParseFail)((p <& q)("cb"))
  it should "fail if right fails" in assertResult(EsoParseFail)((p <& q)("ac"))
  it should "fail if both fail" in assertResult(EsoParseFail)((p <& q)("cc"))
}

class EsoMapParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = EsoRegexParser("a*".r)
  def f(s: String): Int = s.length
  
  "EsoMapParser" should "transform output with f" in {
    assertResult(EsoParsed(1, "bb", 0, 1))((p map f)("abb"))
    assertResult(EsoParsed(2, "b", 0, 2))((p map f)("aab"))}
}

class EsoProdParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = EsoStringParser("p")
  val q: EsoParser[String] = EsoStringParser("q")
  val f: EsoParser[String] = _ => EsoParseFail
  
  "EsoProdParser" should "pass correctly if both p and q pass" in assertResult(EsoParsed(("p", "q"), "rem", 0, 2))((p <&> q)("pqrem"))
  it should "fail if p fails" in assertResult(EsoParseFail)((f <&> q)("q"))
  it should "fail if q fails" in assertResult(EsoParseFail)((p <&> f)("p"))
  it should "fail if p and q fail" in assertResult(EsoParseFail)((f <&> f)(""))
}

class EsoRImpParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = EsoStringParser("p")
  val q: EsoParser[String] = EsoStringParser("q")
  val f: EsoParser[String] = _ => EsoParseFail
  
  "EsoRImpParser" should "pass correctly if both p and q pass" in assertResult(EsoParsed("q", "rem", 0, 2))((p &> q)("pqrem"))
  it should "fail if p fails" in assertResult(EsoParseFail)((f &> q)("q"))
  it should "fail if q fails" in assertResult(EsoParseFail)((p &> f)("p"))
  it should "fail if p and q fail" in assertResult(EsoParseFail)((f &> f)(""))
}

class EsoConstantParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = EsoStringParser("a")
  val f: EsoParser[String] = _ => EsoParseFail
  
  "EsoConstantParser" should "force parser output to a single value" in assertResult(EsoParsed(true, "b", 0, 1))((p ^^^ true)("ab"))
  it should "fail if p fails" in assertResult(EsoParseFail)((f ^^^ true)(""))
}

class EsoAllParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = EsoStringParser("a")
  def ap(num: Int): EsoParser[Vector[String]] = EsoAllParser(p, num)
  
  "EsoAllParser" should "parse all tokens in input" in assertResult(EsoParsed(Vector("a", "a", "a"), "b", 0, 3))(ap(0)("aaab"))
  it should "succeed on none if given 0" in assertResult(EsoParsed(Vector(), "b", 0, 0))(ap(0)("b"))
  it should "fail if number of tokens is below quota" in assertResult(EsoParseFail)(ap(2)("ab"))
}