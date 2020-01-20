package brainfuck

import common.Config
import common_test.EsoSpec

import scala.util.{Success, Try}

class BFISpec extends EsoSpec{
  val hworld: String = grabFile("hworld.b")
  val hworldBugTest: String = grabFile("hworldBugTest.b")
  val bitWidth: String = grabFile("bitWidth.b")
  val eprog: String = grabFile("e.b")
  
  val noDynConfig: Config = defaultConfig.set("init", 100).set("olen", 20)
  val dynConfig: Config = noDynConfig.set("dyn", b=true)
  
  //Naive interpreter
  "BFBase" should "run hworld.b correctly" in {
    val res: Try[String] = getOutputString(BFBase, hworld)
    assertResult(Success("Hello World!\n"))(res)}
  it should "run hworldBugTest.b correctly" in {
    val res: Try[String] = getOutputString(BFBase, hworldBugTest)
    assertResult(Success("Hello World!\n"))(res)}
  it should "run bitWidth.b correctly" in {
    val res: Try[String] = getOutputString(BFBase, bitWidth)
    assertResult(Success("Hello, world!\n"))(res)}
  it should "respect dynamic tape size setting" in {
    lazy val res1 = getOutput(BFBase, eprog, config=noDynConfig) flatMap (l => Try{l.take(20).mkString})
    lazy val res2 = getOutput(BFBase, eprog, config=dynConfig) flatMap (l => Try{l.take(20).mkString})
    if(!res1.isFailure) fail("Tape is not static when dyn is false")
    else if(res2.isFailure) fail("Tape is not dynamic when dyn is true")
    else succeed}
  
  //Optimizing interpreter
  "BFOpt" should "run hworld.b correctly" in {
    val res: Try[String] = getOutputString(BFOpt, hworld)
    assertResult(Success("Hello World!\n"))(res)}
  it should "run hworldBugTest.b correctly" in {
    val res: Try[String] = getOutputString(BFOpt, hworldBugTest)
    assertResult(Success("Hello World!\n"))(res)}
  it should "run bitWidth.b correctly" in {
    val res: Try[String] = getOutputString(BFOpt, bitWidth)
    assertResult(Success("Hello, world!\n"))(res)}
  it should "respect dynamic tape size setting" in {
    lazy val res1 = getOutput(BFOpt, eprog, config=noDynConfig) flatMap (l => Try{l.take(20).mkString})
    lazy val res2 = getOutput(BFOpt, eprog, config=dynConfig) flatMap (l => Try{l.take(20).mkString})
    if(!res1.isFailure) fail("Tape is not static when dyn is false")
    else if(res2.isFailure) fail("Tape is not dynamic when dyn is true")
    else succeed}
  
  //Compiling interpreter
  "BFComp" should "run hworld.b correctly" in {
    val res: Try[String] = getOutputString(BFComp, hworld)
    assertResult(Success("Hello World!\n"))(res)}
  it should "run hworldBugTest.b correctly" in {
    val res: Try[String] = getOutputString(BFComp, hworldBugTest)
    assertResult(Success("Hello World!\n"))(res)}
  it should "run bitWidth.b correctly" in {
    val res: Try[String] = getOutputString(BFComp, bitWidth)
    assertResult(Success("Hello, world!\n"))(res)}
  it should "respect dynamic tape size setting" in {
    lazy val res1 = getOutput(BFComp, eprog, config=noDynConfig) flatMap (l => Try{l.take(20).mkString})
    lazy val res2 = getOutput(BFComp, eprog, config=dynConfig) flatMap (l => Try{l.take(20).mkString})
    if(!res1.isFailure) fail("Tape is not static when dyn is false")
    else if(res2.isFailure) fail("Tape is not dynamic when dyn is true")
    else succeed}
}