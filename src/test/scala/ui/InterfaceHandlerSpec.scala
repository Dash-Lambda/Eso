package ui

import common_test.EsoSpec

import scala.collection.immutable

abstract class InterfaceHandlerSpec extends EsoSpec{
  val defaultState: EsoRunState = EsoRunState.default
}

class SetVarHandlerSpec extends InterfaceHandlerSpec{
  def permuteBin(len: Int): LazyList[Vector[Boolean]] = {
    LazyList
      .from(0)
      .map{n =>
        n.toBinaryString
          .toVector
          .map(_ == '1')}
      .takeWhile(v => v.sizeIs <= len)
      .map{v =>
        v.reverse
          .padTo(len, false)
          .reverse}}
  def applyCases(str: String)(vec: Vector[Boolean]): String = (str.toVector zip vec).map{case (c, b) => if(b) c.toUpper else c.toLower}.mkString
  def permuteCases(str: String): LazyList[String] = permuteBin(str.length).map(applyCases(str))
  
  val trues: LazyList[String] = LazyList.range(1, 10).map(_.toString) #::: LazyList("t", "true", "y", "yes").flatMap(permuteCases)
  val falses: LazyList[String] = "0" #:: LazyList("f", "false", "n", "no").flatMap(permuteCases)
  
  val boolNames: Vector[String] = EsoDefaults.defBoolVec.map(_._1)
  val numNames: Vector[String] = EsoDefaults.defNumVec.map(_._1)
  
  val trueState: EsoRunState = defaultState.setVarSilent("log", "true")
  val falseState: EsoRunState = defaultState.setVarSilent("log", "false")
  val numInitState: EsoRunState = defaultState.setVarSilent("olen", "-1")
  val multiInitState: EsoRunState = (boolNames.map((_, "false")) ++ numNames.map((_, "0"))).foldLeft(defaultState){case (s, (k, v)) => s.setVarSilent(k, v)}
  val multiMapping: immutable.HashMap[String, String] = mkMap(boolNames.map((_, "true")) ++ numNames.map((_, "1")))
  
  "SetVarHandler" should "recognize all true keywords" in {
    for(s <- trues){
      val testState = SetVarHandler(falseState)(immutable.HashMap("log" -> s))
      testState match{
        case EsoRunState(_, _, _, bools, _, _) => assert(bools("log"), s"'$s' gave false")
        case _ => fail("Did Not Return RunState")}}}
  
  it should "recognize all false keywords" in {
    for(s <- falses){
      val testState = SetVarHandler(trueState)(immutable.HashMap("log" -> s))
      testState match{
        case EsoRunState(_, _, _, bools, _, _) => assert(!bools("log"), s"'$s' gave true")
        case _ => fail("Did Not Return RunState")}}}
  
  it should "recognize positive and negative numbers" in {
    for(n <- Seq(-100, -10, 0, 10, 100)){
      val testState = SetVarHandler(numInitState)(immutable.HashMap("olen" -> n.toString))
      testState match{
        case EsoRunState(_, _, _, _, nums, _) => assertResult(nums("olen"))(n)
        case _ => fail("Did Not Return RunState")}}}
  
  it should "recognize character input" in {
    for(c <- 'a' to 'z'){
      val testState = SetVarHandler(numInitState)(immutable.HashMap("olen" -> s"'$c'"))
      testState match{
        case EsoRunState(_, _, _, _, nums, _) => assertResult(nums("olen").toChar)(c)
        case _ => fail("Did Not Return RunState")}}}
  
  it should "recognize multiple assignments" in {
    val testState = SetVarHandler(multiInitState)(multiMapping)
    testState match{
      case EsoRunState(_, _, _, bools, nums, _) =>
        for(nam <- boolNames){
          withClue(s"Failure on variable '$nam'"){
            assertResult(true)(bools(nam))}}
        for(nam <- numNames){
          withClue(s"Failure on variable '$nam'"){
            assertResult(1)(nums(nam))}}
      case _ => fail("Did Not Return RunState")}}
}