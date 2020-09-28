package funge

import java.io.{File, PrintWriter}

import common_test.EsoSpec

import scala.util.Success
import scala.util.matching.Regex

class FungeSpec extends EsoSpec{
  private val badReg: Regex = raw"""\s*(BAD:.*)\z""".r
  val mycology: String = grabFile("mycology.b98")
  def getBadLines(output: String): Vector[String] = output
    .linesIterator
    .collect{case badReg(str) => str}
    .toVector
  
  //Befunge-93
  "Befunge-93" should "pass mycology" in {
    getOutputString(Befunge93, mycology) match{
      case Success(str) =>
        val bads = getBadLines(str)
        assert(bads.isEmpty)
      case _ => fail()}}
  testRTWithFile(Befunge93)("mycology.b98")
  
  //Befunge-98
  "Befunge-98" should "pass mycology" in {
    getOutputString(Befunge98, mycology) match{
      case Success(str) =>
        val bads = getBadLines(str)
        val oFile = new PrintWriter(new File("mycologyReport.txt"))
        oFile.print(str)
        oFile.close()
        if(bads.isEmpty) succeed
        else fail("Mycology Report Contains BAD Lines")
      case _ => fail("Interpreter Failed")}}
  testRTWithFile(Befunge98)("hworld.b98")
}