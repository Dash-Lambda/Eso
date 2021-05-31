package brainfuck

import common_test.EsoSpec

import scala.util.{Failure, Success}

class DFToBFSpec extends EsoSpec{
  val hwdf: String = grabFile("hworld.df")
  
  "DFToBF" should "preserve the behavior of hworld.df" in {
    DFToBF(defaultConfig)(hwdf) match{
      case Failure(e) => fail(s"Transpilation Failed ($e)")
      case Success(hwdfb) => assertOutput(BFOpt, hwdfb, "Hello world")}}
  
  it should "print numbers if dfChar is false" in {
    DFToBF(defaultConfig.set("dfChar", b=false))(hwdf) match{
      case Failure(e) => fail(s"Transpilation Failed ($e)")
      case Success(hwdfb) => assertOutput(BFOpt, hwdfb, "7210110810811132119111114108100")}}
}
