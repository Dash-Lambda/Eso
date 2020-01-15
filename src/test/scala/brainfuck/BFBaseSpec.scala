package brainfuck

class BFBaseSpec extends BFISpec{
  "BFBase" should "run hworld.b correctly" in assert(testProgOutput(BFBase, hworld, hworldRes))
  it should "run hworldBugTest.b correctly" in assert(testProgOutput(BFBase, hworld2, hworldRes))
}