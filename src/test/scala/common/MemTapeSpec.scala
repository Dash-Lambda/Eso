package common

import common_test.EsoSpec

import scala.util.{Success, Try}

class MemTapeSpec extends EsoSpec{
  val statTape: MemTape[Int] = MemTape(Vector(0, 1, 2), dyn=false, 0)
  val dynTape: MemTape[Int] = MemTape(Vector(), dyn=true, 0)
  
  "MemTape" should "respect static size" in assert(Try{statTape(10)}.isFailure)
  it should "respect out-of-bounds gets when dynamic" in {
    Try{dynTape(10)} match{
      case Success(n) => assertResult(0)(n)
      case f => fail(s"Returned $f")}}
  it should "respect out-of-bounds sets when dynamic" in {
    Try{dynTape.set(10, 1)} match {
      case Success(MemTape(vec, _, _)) => assertResult(Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))(vec)
      case f => fail(s"Returned $f")}}
}
