package parsers

import common_test.EsoSpec
import parsers.EsoParser._
import parsers.NewParsers._

class EsoParserSpec extends EsoSpec{
  val as: EsoParser[String] = S("a")
  val bs: EsoParser[String] = S("b")
  val cs: EsoParser[String] = S("c")
  val ar: EsoParser[String] = R("a")
  val br: EsoParser[String] = R("b")
  val cr: EsoParser[String] = R("c")
}

class EsoStringParserSpec extends EsoParserSpec{
  "EsoStringParser" should "parse the first occurrence of its literal" in assertResult(Parsed(("a", "aba", 0, 1)))(S("a")("aba"))
  it should "not parse matches that aren't at the beginning of input" in assertResult(ParseFail)(S("a")("bab"))
  it should "match longer literals" in assertResult(Parsed(("abc", "abcdef", 0, 3)))(S("abc")("abcdef"))
}

class EsoRegexParserSpec extends EsoParserSpec{
  "EsoRegexParser" should "return earliest match" in assertResult(Parsed(("a", "aba", 0, 1)))(R("a".r)("aba"))
  it should "match anywhere in input" in assertResult(Parsed(("a", "bac", 1, 2)))(R("a".r)("bac"))
  it should "return whole match without capture groups" in assertResult(Parsed(("abbc", "aabbcd", 1, 5)))(R("ab*c".r)("aabbcd"))
  it should "return only capture groups if present" in assertResult(Parsed(("ac", "aabbcd", 1, 5)))(R("(a)b*(c)".r)("aabbcd"))
}

class EsoAltParserSpec extends EsoParserSpec{
  val ab: EsoParser[String] = S("ab")
  val abb: EsoParser[String] = S("abb")
  
  "EsoAltParser" should "return the first success" in {
    assertResult(Parsed(("a", "aab", 0, 1)))((ar | br)("aab"))
    assertResult(Parsed(("b", "aab", 2, 3)))((br | ar)("aab"))
    assertResult(Parsed(("a", "abb", 0, 1)))((ar | br)("abb"))
    assertResult(Parsed(("b", "abb", 1, 2)))((br | ar)("abb"))}
  it should "backtrack until success" in {
    assertResult(Parsed(("abc", "abc", 0, 3)))(concat(as | ab, cs)("abc"))}
  it should "backtrack to middle cases" in {
    assertResult(Parsed(("abbc", "abbc", 0, 4)))(concat(abb | ab | as, S("bc"))("abbc"))}
}

class EsoEarliestMatchParserSpec extends EsoParserSpec{
  val ab: EsoParser[String] = R("ab".r)
  
  "EsoEarliestMatchParser" should "return the earliest match" in {
    assertResult(Parsed(("a", "aab", 0, 1)))((ar || br)("aab"))
    assertResult(Parsed(("a", "aab", 0, 1)))((br || ar)("aab"))
    assertResult(Parsed(("a", "abb", 0, 1)))((ar || br)("abb"))
    assertResult(Parsed(("a", "abb", 0, 1)))((br || ar)("abb"))}
  it should "backtrack until success" in {
    assertResult(Parsed(("abc", "aabc", 1, 4)))(concat(ar || ab, cs)("aabc"))}
}

class EsoLongestMatchParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = R("a+".r)
  val q: EsoParser[String] = R("b+".r)
  val ps: EsoParser[String] = S("ab")
  val rs: EsoParser[String] = S("bc")
  
  "EsoLongestMatchParser" should "return the longest match" in {
    assertResult(Parsed(("aa", "aab", 0, 2)))((p ||| q)("aab"))
    assertResult(Parsed(("aa", "aab", 0, 2)))((q ||| p)("aab"))
    assertResult(Parsed(("bb", "abb", 1, 3)))((p ||| q)("abb"))
    assertResult(Parsed(("bb", "abb", 1, 3)))((q ||| p)("abb"))}
  it should "backtrack until success" in {
    assertResult(Parsed(("abc", "abc", 0, 3)))(concat(ps ||| as, rs)("abc"))}
}

class EsoFlatMappedParserSpec extends EsoParserSpec{
  val p: EsoParser[Int] = R("^a*".r) map (_.length)
  def f(n: Int): EsoParser[String] = R("^.".r) map (_*n)
  
  "EsoFlatMappedParser" should "work the do good right" in {
    assertResult(Parsed(("bb", "aabc", 0, 3)))((p flatMap f)("aabc"))}
}

class EsoIntoParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = R(".b".r)
  val q: EsoParser[String] = R("^.".r)
  
  "EsoIntoParser" should "pass p's output into q" in {
    assertResult(Parsed(("a", "cabd", 1, 3)))(into(p, q)("cabd"))}
}

class EsoLCondParserSpec extends EsoParserSpec{
  val q: EsoParser[String] = R("^b".r)
  
  "EsoLCondParser" should "succeed if both succeed" in assertResult(Parsed(("a", "ab", 0, 1)))((ar <| q)("ab"))
  it should "fail if left fails" in assertResult(ParseFail)((ar <| q)("cb"))
  it should "fail if right fails" in assertResult(ParseFail)((ar <| q)("ac"))
  it should "fail if both fail" in assertResult(ParseFail)((ar <| q)("cc"))
}

class EsoLImpParserSpec extends EsoParserSpec{
  val q: EsoParser[String] = R("^b".r)
  
  "EsoLImpParser" should "succeed if both succeed" in assertResult(Parsed(("a", "ab", 0, 2)))((as <& q)("ab"))
  it should "fail if left fails" in assertResult(ParseFail)((as <& q)("cb"))
  it should "fail if right fails" in assertResult(ParseFail)((as <& q)("ac"))
  it should "fail if both fail" in assertResult(ParseFail)((as <& q)("cc"))
}

class EsoMapParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = R("a*".r)
  def f(s: String): Int = s.length
  
  "EsoMapParser" should "transform output with f" in {
    assertResult(Parsed((1, "abb", 0, 1)))((p map f)("abb"))
    assertResult(Parsed((2, "aab", 0, 2)))((p map f)("aab"))}
}

class EsoProdParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = S("p")
  val q: EsoParser[String] = S("q")
  val f: EsoParser[String] = (_,_) => ParseFail
  
  "EsoProdParser" should "pass correctly if both p and q pass" in assertResult(Parsed((("p", "q"), "pqrem", 0, 2)))((p <&> q)("pqrem"))
  it should "fail if p fails" in assertResult(ParseFail)((f <&> q)("q"))
  it should "fail if q fails" in assertResult(ParseFail)((p <&> f)("p"))
  it should "fail if p and q fail" in assertResult(ParseFail)((f <&> f)(""))
}

class EsoRImpParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = S("p")
  val q: EsoParser[String] = S("q")
  val f: EsoParser[String] = (_,_) => ParseFail
  
  "EsoRImpParser" should "pass correctly if both p and q pass" in assertResult(Parsed(("q", "pqrem", 0, 2)))((p &> q)("pqrem"))
  it should "fail if p fails" in assertResult(ParseFail)((f &> q)("q"))
  it should "fail if q fails" in assertResult(ParseFail)((p &> f)("p"))
  it should "fail if p and q fail" in assertResult(ParseFail)((f &> f)(""))
}

class EsoConstantParserSpec extends EsoParserSpec{
  val f: EsoParser[String] = (_,_) => ParseFail
  
  "EsoConstantParser" should "force parser output to a single value" in assertResult(Parsed((true, "ab", 0, 1)))((as ^^^ true)("ab"))
  it should "fail if p fails" in assertResult(ParseFail)((f ^^^ true)(""))
}

class EsoAllParserSpec extends EsoParserSpec{
  def ap(num: Int): EsoParser[Vector[String]] = all(as, num)
  
  "EsoAllParser" should "parse all tokens in input" in assertResult(Parsed((Vector("a", "a", "a"), "aaab", 0, 3)))(ap(0)("aaab"))
  it should "succeed on none if given 0" in assertResult(Parsed((Vector(), "b", 0, 0)))(ap(0)("b"))
  it should "fail if number of tokens is below quota" in assertResult(ParseFail)(ap(2)("ab"))
  it should "backtrack if necessary" in assertResult(Parsed(((Vector("a", "a"), "ab"), "aaab", 0, 4)))((as.* <&> concat(as, bs))("aaab"))
}

class EsoConcatParserSpec extends EsoParserSpec{
  val p: EsoParser[String] = as ^^^ "x"
  val q: EsoParser[String] = bs ^^^ "y"
  
  "EsoConcatParser" should "correctly combine string output" in assertResult(Parsed(("xy", "ab", 0, 2)))(concat(p, q)("ab"))
  it should "fail if p fails" in assertResult(ParseFail)(concat(p, q)("bb"))
  it should "fail if q fails" in assertResult(ParseFail)(concat(p, q)("aa"))
}