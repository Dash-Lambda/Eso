package funge

import common.EsoObj

import scala.collection.immutable

object BF98Lib extends EsoObj{
  val fpvec: Vector[Fingerprint] = Vector[Fingerprint](BOOL, ROMA, MODU)
  val lib: immutable.HashMap[Int, Fingerprint] = mkMap(fpvec.map(fp => (fp.id, fp)))
  
  def apply(id: Int): Fingerprint = lib(id)
  def get(id: Int): Option[Fingerprint] = lib.get(id)
}

trait Fingerprint {
  val name: String
  lazy val id: Int = name.toVector.map(_.toInt).foldLeft(0: Int){case (ac, n) => (ac*256) + n}
  val binds: Vector[(Char, (BF98Prog, Seq[Char], FIP) => FIPRet)]
  
  def push(n: Int)(prog: BF98Prog, inp: Seq[Char], fip: FIP): FIPRet = fip match{
    case FIP(id, ip, dt, so, bs, stk, binds) => FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, dt), dt, so, bs, (n +: fip.TOSS) +: stk.tail, binds))}
  def tossOp(f: FungeStack => FungeStack)(prog: BF98Prog, inp: Seq[Char], fip: FIP): FIPRet = fip match{
    case FIP(id, ip, dt, so, bs, stk, binds) => FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, dt), dt, so, bs, f(fip.TOSS) +: stk.tail, binds))}
}

object BOOL extends Fingerprint{
  val name: String = "BOOL"
  
  def AND(lst: FungeStack): FungeStack = lst match{
    case b #-: a #-: ns => (a&b) +: ns}
  def NOT(lst: FungeStack): FungeStack = lst match{
    case n #-: ns => (~n) +: ns}
  def OR(lst: FungeStack): FungeStack = lst match{
    case b #-: a #-: ns => (a|b) +: ns}
  def XOR(lst: FungeStack): FungeStack = lst match{
    case b #-: a #-: ns => (a^b) +: ns}
  
  val binds: Vector[(Char, (BF98Prog, Seq[Char], FIP) => FIPRet)] = Vector(
    'A' -> tossOp(AND),
    'N' -> tossOp(NOT),
    'O' -> tossOp(OR),
    'X' -> tossOp(XOR))
}

object ROMA extends Fingerprint{
  val name: String = "ROMA"
  
  val binds: Vector[(Char, (BF98Prog, Seq[Char], FIP) => FIPRet)] = Vector(
    'C' -> push(100),
    'D' -> push(500),
    'I' -> push(1),
    'L' -> push(50),
    'M' -> push(1000),
    'V' -> push(5),
    'X' -> push(10))
}

object MODU extends Fingerprint{
  val name: String = "MODU"
  
  def tossOpDiv(f: (Boolean, FungeStack) => FungeStack): (BF98Prog, Seq[Char], FIP) => FIPRet = (prog, seq, fip) => tossOp(f(prog.bDiv, _))(prog, seq, fip)
  
  def MMod(bDiv: Boolean, lst: FungeStack): FungeStack = lst match{
    case b #-: a #-: ns => (if(b == 0 && bDiv) 0 else (a%b + b)%b) +: ns}
  def UMod(bDiv: Boolean, lst: FungeStack): FungeStack = lst match{
    case b #-: a #-: ns => (if(b == 0 && bDiv) 0 else a%b).abs +: ns}
  def RMod(bDiv: Boolean, lst: FungeStack): FungeStack = lst match{
    case b #-: a #-: ns => (if(b == 0 && bDiv) 0 else a%b) +: ns}
  
  val binds: Vector[(Char, (BF98Prog, Seq[Char], FIP) => FIPRet)] = Vector(
    'M' -> tossOpDiv(MMod),
    'U' -> tossOpDiv(UMod),
    'R' -> tossOpDiv(RMod))
}