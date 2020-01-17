package whitespace

import common_test.EsoSpec

import scala.util.Success

class WSAssemblySpec extends EsoSpec{
  val hworldwsa: String = grabFile("hworld.wsa")
  val hworldws: String = grabFile("hworld.ws")
  
  "WSAssembly" should "preserve the behavior of hworld.wsa" in {
    WSAssembly(defaultConfig)(hworldwsa) match{
      case Success(hwws) =>
        val res = getOutputString(WhiteSpace, hwws)
        assertResult(Success("Hello, world!"))(res)
      case f => fail(s"Returned $f")}}
  
  it should "preserve the structure of hworld.ws" in {
    val res = WSAssembly.unapply(defaultConfig)(hworldws)
    assertResult(Success(hworldwsa))(res)}
}
