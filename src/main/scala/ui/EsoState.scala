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
                       binds: immutable.HashMap[String, String]) extends EsoState{
  import EsoRunState.{trueReg, falseReg, charReg, intReg}
  
  def addInterp(intp: Interpreter): EsoRunState = EsoRunState(interps + ((intp.name, intp)), gens, trans, bools, nums, binds)
  def addGen(gen: Transpiler): EsoRunState = EsoRunState(interps, gens + ((gen.id, gen)), trans, bools, nums, binds)
  def addTrans(tran: Translator): EsoRunState = EsoRunState(interps, gens, trans + ((tran.id, tran)), bools, nums, binds)
  def addBool(nam: String, bool: Boolean): EsoRunState = EsoRunState(interps, gens, trans, bools + ((nam, bool)), nums, binds)
  def addNum(nam: String, num: Int): EsoRunState = EsoRunState(interps, gens, trans, bools, nums + ((nam, num)), binds)
  def addBind(nam: String, bind: String): EsoRunState = EsoRunState(interps, gens, trans, bools, nums, binds + ((nam, bind)))
  
  def addAllTrans(tranVec: Vector[Translator]): EsoRunState = EsoRunState(interps, gens, trans ++ tranVec.map(t => (t.id, t)), bools, nums, binds)
  
  def dropInterp(nam: String): EsoRunState = EsoRunState(interps - nam, gens, trans, bools, nums, binds)
  def dropGen(nam: (String, String)): EsoRunState = EsoRunState(interps, gens - nam, trans, bools, nums, binds)
  def dropTran(nam: (String, String)): EsoRunState = EsoRunState(interps, gens, trans - nam, bools, nums, binds)
  def dropBool(nam: String): EsoRunState = EsoRunState(interps, gens, trans, bools - nam, nums, binds)
  def dropNum(nam: String): EsoRunState = EsoRunState(interps, gens, trans, bools, nums - nam, binds)
  def dropBind(nam: String): EsoRunState = EsoRunState(interps, gens, trans, bools, nums, binds - nam)
  
  def clearBinds: EsoRunState = EsoRunState(interps, gens, trans, bools, nums, immutable.HashMap[String, String]())
  
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
    case _ => Failure(EsoExcep(s"""Unrecognized Variable "$k""""))}
  
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
  val trueReg: Regex = raw"""(?i)(?:true|yes|t|y|[1-9])""".r
  val falseReg: Regex = raw"""(?i)(?:false|no|f|n|0)""".r
  val charReg: Regex = raw"""'(.)'""".r
  val intReg: Regex = raw"""-?\d+""".r
  
  val default: EsoRunState = EsoRunState(
    mkMap(EsoDefaults.defInterpVec.map(i => (i.name, i))),
    mkMap(EsoDefaults.defGenVec.map(g => (g.id, g))),
    mkMap(EsoDefaults.defTransVec.map(t => (t.id, t))),
    mkMap(EsoDefaults.defBoolVec.map(t => (t._1, t._2))),
    mkMap(EsoDefaults.defNumVec.map(t => (t._1, t._2))),
    mkMap(Vector()))
  
  @tailrec
  def withOps(str: String, state: EsoRunState = default): EsoRunState = str match{
    case numReg(k, v, ops) if state.nums.isDefinedAt(k) => withOps(ops, state.setVarSilent(k, v))
    case boolReg(k, v, ops) if state.bools.isDefinedAt(k) => withOps(ops, state.setVarSilent(k, v))
    case flagReg(k, ops) if state.bools.isDefinedAt(k) => withOps(ops, state.setVarSilent(k, "true"))
    case biteReg(ops) => withOps(ops, state)
    case _ => state}
}