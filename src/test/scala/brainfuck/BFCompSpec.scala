package brainfuck

class BFCompSpec extends BFISpec{
  "BFComp" should "run hworld.b correctly" in assert(testProgOutput(BFComp, hworld, hworldRes))
}