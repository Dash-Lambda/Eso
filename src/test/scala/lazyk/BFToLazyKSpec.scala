package lazyk

import common_test.EsoSpec

import scala.util.{Failure, Success}

class BFToLazyKSpec extends EsoSpec{
  "BFToLazyK" should "preserve the behavior of bitWidth.b" in {
    val bwb = grabFile("bitWidth.b")
    BFToLazyK(defaultConfig)(bwb) match{
      case Failure(e) => fail(s"Transpilation Failed ($e)")
      case Success(bwl) => assertOutput(LazyK, bwl, "Hello, world! 254\n")}}
}
