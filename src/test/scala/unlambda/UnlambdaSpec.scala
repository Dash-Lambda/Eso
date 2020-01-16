package unlambda

import common_test.EsoSpec

import scala.util.{Success, Try}

class UnlambdaSpec extends EsoSpec{
  val hworld: String = grabFile("hworld.unl")
  val res: Try[String] = getOutputString(Unlambda, hworld)
  
  "Unlambda" should "run hworld.unl correctly" in assertResult(Success("Hello world\n"))(res)
}
