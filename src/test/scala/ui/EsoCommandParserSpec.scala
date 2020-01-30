package ui

import common_test.EsoSpec

import scala.collection.immutable

class EsoCommandParserSpec extends EsoSpec{
  "EsoCommandParser" should "fail on empty input" in {
    val res1 = EsoCommandParser("")
    assertResult(ParseFail)(res1)
    val res2 = EsoCommandParser(" \t\n")
    assertResult(ParseFail)(res2)}
  
  it should "recognize unary commands" in {
    val res1 = EsoCommandParser("test")
    assertResult(EsoCmd("test", immutable.HashMap()))(res1)
    val res2 = EsoCommandParser("test2  \t")
    assertResult(EsoCmd("test2", immutable.HashMap()))(res2)}
  
  it should "recognize commands followed by an argument" in {
    val res1 = EsoCommandParser("test -a A")
    res1 match{
      case EsoCmd(cmd, args) =>
        assertResult("test")(cmd)
        withClue(s"args=$args"){
          assertResult(Some("A"))(args.get("a"))}
      case _ => fail("Parse Failed")}}
  
  it should "recognize commands followed by multiple arguments" in {
    val argRef = Vector("a"->"A", "b"->"B", "op"->"Option")
    val str1 = s"test ${argRef.map{case (k, v) => s"-$k $v"}.mkString(" ")}"
    val res1 = EsoCommandParser(str1)
    res1 match{
      case EsoCmd(cmd, args) =>
        assertResult("test")(cmd)
        withClue(s"args=$args"){
          for((k, v) <- argRef){
            assertResult(Some(v))(args.get(k))}}
      case _ => fail("Parse Failed")}}
}
