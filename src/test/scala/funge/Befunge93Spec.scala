package funge

class Befunge93Spec extends FungeSpec{
  "Funge-93" should "pass mycology" in assert(testMycology(Befunge93))
}