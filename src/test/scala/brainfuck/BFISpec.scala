package brainfuck

import common_test.EsoSpec

import scala.util.{Success, Try}

class BFISpec extends EsoSpec{
  val hworld: String = grabFile("hworld.b")
  val hworldBugTest: String = grabFile("hworldBugTest.b")
  val bitWidth: String = grabFile("bitWidth.b")
  
  //Naive interpreter
  lazy val baseRes1: Try[String] = getOutputString(BFBase, hworld)
  lazy val baseRes2: Try[String] = getOutputString(BFBase, hworldBugTest)
  lazy val baseRes3: Try[String] = getOutputString(BFBase, bitWidth)
  "BFBase" should "run hworld.b correctly" in assertResult(Success("Hello World!\n"))(baseRes1)
  it should "run hworldBugTest.b correctly" in assertResult(Success("Hello World!\n"))(baseRes2)
  it should "run bitWidth.b correctly" in assertResult(Success("Hello, world!\n"))(baseRes3)
  
  //Optimizing interpreter
  lazy val optRes1: Try[String] = getOutputString(BFOpt, hworld)
  lazy val optRes2: Try[String] = getOutputString(BFOpt, hworldBugTest)
  lazy val optRes3: Try[String] = getOutputString(BFOpt, bitWidth)
  "BFOpt" should "run hworld.b correctly" in assertResult(Success("Hello World!\n"))(optRes1)
  it should "run hworldBugTest.b correctly" in assertResult(Success("Hello World!\n"))(optRes2)
  it should "run bitWidth.b correctly" in assertResult(Success("Hello, world!\n"))(optRes3)
  
  //Compiling interpreter
  lazy val compRes1: Try[String] = getOutputString(BFComp, hworld)
  lazy val compRes2: Try[String] = getOutputString(BFComp, hworldBugTest)
  lazy val compRes3: Try[String] = getOutputString(BFComp, bitWidth)
  "BFComp" should "run hworld.b correctly" in assertResult(Success("Hello World!\n"))(compRes1)
  it should "run hworldBugTest.b correctly" in assertResult(Success("Hello World!\n"))(compRes2)
  it should "run bitWidth.b correctly" in assertResult(Success("Hello, world!\n"))(compRes3)
}