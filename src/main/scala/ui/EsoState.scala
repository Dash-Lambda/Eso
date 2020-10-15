package ui

import common.{Config, EsoExcep, EsoObj, Interpreter, Translator, Transpiler}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

trait EsoState extends EsoObj
object EsoHalt extends EsoState
case class EsoRunState(interps: immutable.HashMap[String, Interpreter],
                       gens: immutable.HashMap[(String, String), Transpiler],
                       trans: immutable.HashMap[(String, String), Translator],
                       bools: immutable.HashMap[String, Boolean],
                       nums: immutable.HashMap[String, Int],
                       binds: immutable.HashMap[String, String],
                       runCache: immutable.HashMap[(String, String), (Seq[Char] => LazyList[Char], Long)]) extends EsoState{
  import EsoRunState.{trueReg, falseReg, charReg, intReg}
  
  def addInterp(intp: Interpreter): EsoRunState = nextState(newInterps = interps + ((intp.name, intp)))
  def addGen(gen: Transpiler): EsoRunState = nextState(newGens = gens + ((gen.id, gen)))
  def addTrans(tran: Translator): EsoRunState = nextState(newTrans = trans + ((tran.id, tran)))
  def addBool(nam: String, bool: Boolean): EsoRunState = nextState(newBools = bools + ((nam, bool)))
  def addNum(nam: String, num: Int): EsoRunState = nextState(newNums = nums + ((nam, num)))
  def addBind(nam: String, bind: String): EsoRunState = nextState(newBinds = binds + ((nam, bind)))
  def cacheFunc(nam: String, lang: String, lm: Long, func: Seq[Char] => LazyList[Char]): EsoRunState = nextState(newRunCache = runCache + (((nam, lang), (func, lm))))
  
  def addAllTrans(tranVec: Vector[Translator]): EsoRunState = nextState(newTrans = trans ++ tranVec.map(t => (t.id, t)))
  
  def dropInterp(nam: String): EsoRunState = nextState(newInterps = interps - nam)
  def dropGen(nam: (String, String)): EsoRunState = nextState(newGens = gens - nam)
  def dropTran(nam: (String, String)): EsoRunState = nextState(newTrans = trans - nam)
  def dropBool(nam: String): EsoRunState = nextState(newBools = bools - nam)
  def dropNum(nam: String): EsoRunState = nextState(newNums = nums - nam)
  def dropBind(nam: String): EsoRunState = nextState(newBinds = binds - nam)
  
  def clearBinds: EsoRunState = nextState(newBinds = immutable.HashMap())
  def clearCache: EsoRunState = nextState(newRunCache = immutable.HashMap())
  
  def nextState(newInterps: immutable.HashMap[String, Interpreter] = interps,
                newGens: immutable.HashMap[(String, String), Transpiler] = gens,
                newTrans: immutable.HashMap[(String, String), Translator] = trans,
                newBools: immutable.HashMap[String, Boolean] = bools,
                newNums: immutable.HashMap[String, Int] = nums,
                newBinds: immutable.HashMap[String, String] = binds,
                newRunCache: immutable.HashMap[(String, String), (Seq[Char] => LazyList[Char], Long)] = runCache): EsoRunState = {
    EsoRunState(newInterps, newGens, newTrans, newBools, newNums, newBinds, newRunCache)
  }
  
  def getTrans(sl: String, tl: String): Option[Config => String => Try[String]] = trans.get((sl, tl)) match{
    case Some(t) => Some(t.apply)
    case None => trans.get((tl, sl)) match{
      case Some(t) => Some(t.unapply)
      case None => None}}
  
  def setVarSilent(k: String, v: String): EsoRunState = setVar(k, v).getOrElse(this)
  def setVar(k: String, v: String): Try[EsoRunState] = v match{
    case trueReg() if bools.isDefinedAt(k) => Success(addBool(k, bool=true))
    case falseReg() if bools.isDefinedAt(k) => Success(addBool(k, bool=false))
    case intReg() if nums.isDefinedAt(k) => Success(addNum(k, v.toInt))
    case charReg(c) if nums.isDefinedAt(k) => Success(addNum(k, c(0).toInt))
    case _ => Failure(EsoExcep(s"""Invalid Parameter Name or Value for "$k""""))}
  
  def config: Config = Config(bools, nums)
  def interpNames: Vector[String] = interps.keys.toVector
  def genNames: Vector[(String, String)] = gens.keys.toVector
  def genLinks: immutable.HashMap[String, Vector[String]] = linkUp(gens.keys.toVector)
  def transLinks: immutable.HashMap[String, Vector[String]] = linkUp((trans.keys ++ trans.keys.map{case (k, v) => (v, k)}).toVector)
  def linkUp(links: Vector[(String, String)]): immutable.HashMap[String, Vector[String]] = {
    @tailrec
    def ldo(src: Vector[(String, String)], lnk: immutable.HashMap[String, Vector[String]]): immutable.HashMap[String, Vector[String]] = src match{
      case (k, v) +: ps => lnk.get(k) match{
        case Some(vec) => ldo(ps, lnk + ((k, vec :+ v)))
        case None => ldo(ps, lnk + ((k, Vector(v))))}
      case _ => lnk}
    ldo(links, immutable.HashMap())}
}
object EsoRunState extends EsoObj{
  val numReg: Regex = raw"""[^-]*-(\w+) (-?\d+)(.*)\z""".r
  val boolReg: Regex = raw"""[^-]*-(\w+) ((?:true|false))(.*)\z""".r
  val flagReg: Regex = raw"""[^-]*-(\w+)(.*)\z""".r
  val biteReg: Regex = raw"""[^-]*-(.*)\z""".r
  val trueReg: Regex = raw"""(?i)(?:true|yes|t|y|[1-9]|on|i)""".r
  val falseReg: Regex = raw"""(?i)(?:false|no|f|n|0|off|o)""".r
  val charReg: Regex = raw"""'(.)'""".r
  val intReg: Regex = raw"""-?\d+""".r
  
  val default: EsoRunState = EsoRunState(
    mkMap(EsoDefaults.defInterpVec.map(i => (i.name, i))),
    mkMap(EsoDefaults.defGenVec.map(g => (g.id, g))),
    mkMap(EsoDefaults.defTransVec.map(t => (t.id, t))),
    mkMap(EsoDefaults.defBoolVec.map(t => (t._1, t._2))),
    mkMap(EsoDefaults.defNumVec.map(t => (t._1, t._2))),
    immutable.HashMap(),
    immutable.HashMap())
  
  def withOps(opstr: String, initState: EsoRunState = default): EsoRunState = {
    @tailrec
    def wdo(str: String, state: EsoRunState = default): EsoRunState = str match{
      case numReg(k, v, ops) if state.nums.isDefinedAt(k) => wdo(ops, state.setVarSilent(k, v))
      case boolReg(k, v, ops) if state.bools.isDefinedAt(k) => wdo(ops, state.setVarSilent(k, v))
      case flagReg(k, ops) if state.bools.isDefinedAt(k) => wdo(ops, state.setVarSilent(k, "true"))
      case biteReg(ops) => wdo(ops, state)
      case _ => state}
    wdo(opstr, initState)}
  
  def withOps(ops: (String, String)*): EsoRunState = withOpSeq(ops.toVector)
  def withOpSeq(ops: Seq[(String, String)]): EsoRunState = {
    ops.foldLeft(default){
      case (s, (k, v)) => s.setVarSilent(k, v)}}
}