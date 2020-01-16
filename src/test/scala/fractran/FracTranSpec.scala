package fractran

import common_test.EsoSpec

import scala.util.{Success, Try}

class FracTranSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.ftp")
  val primes: String = grabFile("primes.ft")
  
  lazy val ftpRes: Try[String] = getOutputString(FracTranpp, hworld)
  "FracTran++" should "run hworld.ftp correctly" in assertResult(Success("Hello World"))(ftpRes)
  
  lazy val ftRes: Try[String] = getOutput(FracTran, primes) map (r => r.take(94).mkString)
  "FracTran" should "run primes.ft correctly" in assertResult(Success("2\n15\n825\n725\n1925\n2275\n425\n390\n330\n290\n770\n910\n170\n156\n132\n116\n308\n364\n68\n4\n30\n225\n12375\n10875"))(ftRes)
}