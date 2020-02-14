package fractran

import common.PrimeNumTools
import common_test.EsoSpec

class FracTranSpec extends EsoSpec{
  testAllAgainstOutputAutoLimit(FracTranpp)(
    ("hworld.ftp", "", "Hello World", false),
    ("primes.ftp", "", PrimeNumTools.primesLazy.take(25).mkString("\n"), true))
  
  "FracTran" should "run primes.ft correctly" in assertOutputAutoLimit(FracTran, grabFile("primes.ft"), "2\n15\n825\n725\n1925\n2275\n425\n390\n330\n290\n770\n910\n170\n156\n132\n116\n308\n364\n68\n4\n30\n225\n12375\n10875")
}