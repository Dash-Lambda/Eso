package wierd

import common_test.EsoSpec

import scala.util.{Success, Try}

class WierdSpec extends EsoSpec{
  val quine: String = grabFile("quine.wd")
  val res: Try[String] = getOutputString(Wierd, quine)
  
  "Wierd" should "run quine.wd correctly" in assertResult(Success(quine))(res)
}
