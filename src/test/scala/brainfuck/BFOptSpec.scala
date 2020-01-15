package brainfuck

class BFOptSpec extends BFISpec{
  "BFOpt" should "run hworld.b correctly" in assert(testProgOutput(BFOpt, hworld, hworldRes))
}