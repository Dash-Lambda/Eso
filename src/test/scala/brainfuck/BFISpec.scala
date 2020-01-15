package brainfuck

import common.Interpreter
import common_test.EsoSpec

abstract class BFISpec extends EsoSpec{
  val hworld: String = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
  val hworldRes: String = "Hello World!\n"
  
  def testProgOutput(bi: Interpreter, prog: String, res: String, inp: Seq[Char] = Seq()): Boolean = testInterp(bi, defaultConfig, prog, inp)(outputEquals(res))
}