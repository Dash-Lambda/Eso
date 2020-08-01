package brainfuck

import common.Config
import common_test.EsoSpec

import scala.util.Try

class BFISpec extends EsoSpec{
  val eprog: String = grabFile("e.b")
  val noDynConfig: Config = defaultConfig.set("init", 100).set("olen", 20)
  val dynConfig: Config = noDynConfig.set("dyn", b=true)
  
  //Naive interpreter
  testAllAgainstOutput(BFBase)(
    ("hworld.b", "", "Hello World!\n"),
    ("hworldBugTest.b", "", "Hello World!\n"),
    ("bitWidth.b", "", "Hello, world!\n"))
  it should "respect dynamic tape size setting" in {
    lazy val res1 = getOutput(BFBase, eprog, config=noDynConfig) flatMap (l => Try{l.take(20).mkString})
    lazy val res2 = getOutput(BFBase, eprog, config=dynConfig) flatMap (l => Try{l.take(20).mkString})
    if(!res1.isFailure) fail("Tape is not static when dyn is false")
    else if(res2.isFailure) fail("Tape is not dynamic when dyn is true")
    else succeed}
  
  //Optimizing interpreter
  testAllAgainstOutput(BFOpt)(
    ("hworld.b", "", "Hello World!\n"),
    ("hworldBugTest.b", "", "Hello World!\n"),
    ("bitWidth.b", "", "Hello, world!\n"),
    ("lostKingdom.b", "y\nn\nw\nc5\nn\n", grabFile("lostKingdomLog.txt")))
  it should "respect dynamic tape size setting" in {
    lazy val res1 = getOutput(BFOpt, eprog, config=noDynConfig) flatMap (l => Try{l.take(20).mkString})
    lazy val res2 = getOutput(BFOpt, eprog, config=dynConfig) flatMap (l => Try{l.take(20).mkString})
    if(!res1.isFailure) fail("Tape is not static when dyn is false")
    else if(res2.isFailure) fail("Tape is not dynamic when dyn is true")
    else succeed}
  
  //Compiling interpreter
  testAllAgainstOutput(BFComp)(
    ("hworld.b", "", "Hello World!\n"),
    ("hworldBugTest.b", "", "Hello World!\n"),
    ("bitWidth.b", "", "Hello, world!\n"),
    ("mandelbrot.b", "", grabFile("mandelbrotOutput.txt")))
  it should "respect dynamic tape size setting" in {
    lazy val res1 = getOutput(BFComp, eprog, config=noDynConfig) flatMap (l => Try{l.take(20).mkString})
    lazy val res2 = getOutput(BFComp, eprog, config=dynConfig) flatMap (l => Try{l.take(20).mkString})
    if(!res1.isFailure) fail("Tape is not static when dyn is false")
    else if(res2.isFailure) fail("Tape is not dynamic when dyn is true")
    else succeed}
}