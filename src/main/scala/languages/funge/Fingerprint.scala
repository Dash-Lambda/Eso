package languages.funge

import common.EsoObj

import scala.collection.immutable


object BF98Lib extends EsoObj{
  val fpvec: Vector[Fingerprint] = Vector[Fingerprint](BOOL, ROMA, MODU, NULL, HRTI, REFC, CPLI)
  val lib: immutable.HashMap[Int, Fingerprint] = mkMap(fpvec.map(fp => (fp.id, fp)))
  
  def apply(id: Int): Fingerprint = lib(id)
  def get(id: Int): Option[Fingerprint] = lib.get(id)
}

trait Fingerprint {
  val name: String
  lazy val id: Int = name.toVector.map(_.toInt).foldLeft(0: Int){case (ac, n) => (ac*256) + n}
  val binds: Vector[(Char, (BF98Prog, BF98State, FIP) => FIPRet)]
  
  def pushToToss(fip: FIP)(ns: Int*): FIP = fip match{
    case FIP(fid, ip, dt, so, bs, stk, binds) => stk match{
      case toss +: ss => FIP(fid, ip, dt, so, bs, (ns ++: toss) +: ss, binds)}}
  def pushToToss(stk: Vector[FungeStack])(ns: Int*): Vector[FungeStack] = stk match{
    case toss +: tl => (ns ++: toss) +: tl}
  
  def reflect(prog: BF98Prog, dat: BF98State, fip: FIP): FIPRet = fip match{
    case FIP(fid, ip, dt, so, bs, stk, binds) => FIPCont(prog, dat, FIP(fid, prog.getNextInd(ip, -dt), -dt, so, bs, stk, binds))}
  
  def push(n: Int)(prog: BF98Prog, dat: BF98State, fip: FIP): FIPRet = fip match{
    case FIP(fid, ip, dt, so, bs, stk, binds) => FIPCont(prog, dat, FIP(fid, prog.getNextInd(ip, dt), dt, so, bs, (n +: fip.TOSS) +: stk.tail, binds))}
  def tossOp(f: FungeStack => FungeStack)(prog: BF98Prog, dat: BF98State, fip: FIP): FIPRet = fip match{
    case FIP(fid, ip, dt, so, bs, stk, binds) => FIPCont(prog, dat, FIP(fid, prog.getNextInd(ip, dt), dt, so, bs, f(fip.TOSS) +: stk.tail, binds))}
  def fpDatOp(f: BF98FPData => BF98FPData)(prog: BF98Prog, dat: BF98State, fip: FIP): FIPRet = dat match{
    case BF98State(times, inp, rand, fpDat) => FIPCont(prog, BF98State(times, inp, rand, f(fpDat)), fip.step(prog))}
  def progOp(f: BF98Prog => BF98Prog)(prog: BF98Prog, dat: BF98State, fip: FIP): FIPRet = {
    val nprog = f(prog)
    FIPCont(nprog, dat, fip.step(nprog))}
  def tossOutOp(f: FungeStack => (String, FungeStack))(prog: BF98Prog, dat: BF98State, fip: FIP): FIPRet = fip match{
    case FIP(fid, ip, dt, so, bs, toss +: tl, binds) => f(toss) match{
      case(str, ntoss) => FIPOut(str, prog, dat, FIP(fid, prog.getNextInd(ip, dt), dt, so, bs, ntoss +: tl, binds))}}
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
  
  val binds: Vector[(Char, (BF98Prog, BF98State, FIP) => FIPRet)] = Vector(
    'A' -> tossOp(AND),
    'N' -> tossOp(NOT),
    'O' -> tossOp(OR),
    'X' -> tossOp(XOR))
}

object ROMA extends Fingerprint{
  val name: String = "ROMA"
  
  val binds: Vector[(Char, (BF98Prog, BF98State, FIP) => FIPRet)] = Vector(
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
  
  def tossOpDiv(f: (Boolean, FungeStack) => FungeStack): (BF98Prog, BF98State, FIP) => FIPRet = (prog, seq, fip) => tossOp(f(prog.bDiv, _))(prog, seq, fip)
  
  def MMod(bDiv: Boolean, lst: FungeStack): FungeStack = lst match{
    case b #-: a #-: ns => (if(b == 0 && bDiv) 0 else (a%b + b)%b) +: ns}
  def UMod(bDiv: Boolean, lst: FungeStack): FungeStack = lst match{
    case b #-: a #-: ns => (if(b == 0 && bDiv) 0 else a%b).abs +: ns}
  def RMod(bDiv: Boolean, lst: FungeStack): FungeStack = lst match{
    case b #-: a #-: ns => (if(b == 0 && bDiv) 0 else a%b) +: ns}
  
  val binds: Vector[(Char, (BF98Prog, BF98State, FIP) => FIPRet)] = Vector(
    'M' -> tossOpDiv(MMod),
    'U' -> tossOpDiv(UMod),
    'R' -> tossOpDiv(RMod))
}

object NULL extends Fingerprint{
  val name: String = "NULL"
  override lazy val id: Int = 0x4e554c4c
  
  val binds: Vector[(Char, (BF98Prog, BF98State, FIP) => FIPRet)] = ('A' to 'Z').toVector.map(c => (c, reflect))
}

object HRTI extends Fingerprint{
  val name: String = "HRTI"
  
  def gran(stk: FungeStack): FungeStack = 1000 +: stk
  def mark(prog: BF98Prog, dat: BF98State, fip: FIP): FIPRet = dat.readTime match{
    case (t, ndat) => fpDatOp(_.markTime(fip.id, t))(prog, ndat, fip)}
  def timer(prog: BF98Prog, dat: BF98State, fip: FIP): FIPRet = dat.fpDat.hrtiMarks.get(fip.id) match{
    case None => reflect(prog, dat, fip)
    case Some(t0) => dat.readTime match{
      case (t1, ndat) => FIPCont(prog, ndat, pushToToss(fip)(((t1 - t0)*1000).toInt).step(prog))}}
  def unmark(prog: BF98Prog, dat: BF98State, fip: FIP): FIPRet = fpDatOp(_.unMarkTime(fip.id))(prog, dat, fip)
  def seconds(prog: BF98Prog, dat: BF98State, fip: FIP): FIPRet = dat.readTime match{
    case (t, ndat) => FIPCont(prog, ndat, pushToToss(fip)(((t%1000)*1000).toInt).step(prog))}
  
  val binds: Vector[(Char, (BF98Prog, BF98State, FIP) => FIPRet)] = Vector(
    'G' -> tossOp(gran),
    'M' -> mark,
    'T' -> timer,
    'E' -> unmark,
    'S' -> seconds)
}

object REFC extends Fingerprint{
  val name: String = "REFC"
  
  def ref(prog: BF98Prog, dat: BF98State, fip: FIP): FIPRet = fip match{
    case FIP(fid, ip, dt, so, bs, stk, binds) => dat match{
      case BF98State(times, inp, rand, fpDat) => stk match{
        case toss +: ss => toss match{
          case y #-: x #-: ns => fpDat.pushRef(y, x) match{
            case (i, nfpd) => FIPCont(prog, BF98State(times, inp, rand, nfpd), FIP(fid, ip, dt, so, bs, (i +: ns) +: ss, binds).step(prog))}}}}}
  
  def deref(prog: BF98Prog, dat: BF98State, fip: FIP): FIPRet = fip match{
    case FIP(fid, ip, dt, so, bs, stk, binds) => stk match{
      case toss +: ss => toss match{
        case n #-: ns => dat.fpDat.vecRefs(n) match{
          case (x, y) => FIPCont(prog, dat, FIP(fid, ip, dt, so, bs, (y +: x +: ns) +: ss, binds).step(prog))}}}}
  
  val binds: Vector[(Char, (BF98Prog, BF98State, FIP) => FIPRet)] = Vector(
    'R' -> ref,
    'D' -> deref)
}

object CPLI extends Fingerprint{
  val name: String = "CPLI"
  
  def add(stk: FungeStack): FungeStack = stk match{
    case bi #-: br #-: ai #-: ar #-: tl => (ai + bi) +: (ar + br) +: tl}
  def subt(stk: FungeStack): FungeStack = stk match{
    case bi #-: br #-: ai #-: ar #-: tl => (ai - bi) +: (ar - br) +: tl}
  def mult(stk: FungeStack): FungeStack = stk match{
    case bi #-: br #-: ai #-: ar #-: tl => (ar*bi + ai*br) +: (ar*br - ai*bi) +: tl}
  def div(prog: BF98Prog, state: BF98State, fip: FIP): FIPRet = {
    def ddo(stk: FungeStack): FungeStack = stk match{
      case bi #-: br #-: ai #-: ar #-: tl =>
        if((br*br - bi*bi) == 0 && prog.bDiv) 0 +: 0 +: tl
        else ((ai*br - ar*bi)/(br*br + bi*bi)) +: ((ar*br + ai*bi)/(br*br + bi*bi)) +: tl}
    tossOp(ddo)(prog, state, fip)}
  def abs(stk: FungeStack): FungeStack = stk match{
    case i #-: r #-: tl => math.sqrt(r*r + i*i).toInt +: tl}
  def printComp(stk: FungeStack): (String, FungeStack) = stk match{
    case i #-: r #-: tl => (s"$r${if(i >= 0) "+" else "-"}${i.abs}i", tl)}
  
  val binds: Vector[(Char, (BF98Prog, BF98State, FIP) => FIPRet)] = Vector(
    'A' -> tossOp(add),
    'S' -> tossOp(subt),
    'M' -> tossOp(mult),
    'D' -> div,
    'V' -> tossOp(abs),
    'O' -> tossOutOp(printComp))
}