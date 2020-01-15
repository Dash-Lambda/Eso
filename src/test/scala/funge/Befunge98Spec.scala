package funge

class Befunge98Spec extends FungeSpec{
  "Funge-98" should "pass mycology" in assert(testMycology(Befunge98))
}