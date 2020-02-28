package unlambda

import common.PrimeNumTools
import common_test.EsoSpec

class UnlambdaSpec extends EsoSpec{
  testAllAgainstOutputAutoLimit(Unlambda, defaultConfig.set("echoFileInp", b=false))(
    ("hworld.unl", "", "Hello world\n", false),
    ("alphatest.unl", "", "abcdefghijklmnopqrstuvwxyz0123456789\n", false),
    ("adventure.unl", grabFile("winAdventure.txt"), grabFile("adventureRef.txt"), true),
    ("primes.unl", "", PrimeNumTools.primesLazy.take(100).mkString("\n"), true))
}
