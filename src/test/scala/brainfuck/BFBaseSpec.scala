package brainfuck

class BFBaseSpec extends BFISpec{
  "BFBase" should "run hworld.b correctly" in assert(testProgOutput(BFBase, hworld, hworldRes))
}