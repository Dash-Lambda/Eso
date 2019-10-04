package funge

import common.EsoObj

import scala.collection.immutable

object BF98Lib extends EsoObj{
  val fpvec: Vector[Fingerprint] = Vector[Fingerprint](BOOL)
  val lib: immutable.HashMap[Int, Fingerprint] = mkMap(fpvec.map(fp => (fp.id, fp)))
  
  def apply(id: Int): Fingerprint = lib(id)
  def get(id: Int): Option[Fingerprint] = lib.get(id)
}

trait Fingerprint {
  val name: String
  lazy val id: Int = name.toVector.map(_.toInt).foldLeft(0: Int){case (ac, n) => (ac*256) + n}
  val binds: Vector[(Char, (BF98Prog, Seq[Char], FIP) => FIPRet)]
}

object BOOL extends Fingerprint{
  val name: String = "BOOL"
  
  def AND(prog: BF98Prog, inp: Seq[Char], fip: FIP): FIPRet = fip match{
    case FIP(id, ip, dt, so, bs, stk, binds) => fip.TOSS match{
      case b +: a +: ns =>
        val res = a & b
        FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, dt), dt, so, bs, (res +: ns) +: stk.tail, binds))
    }
  }
  
  def NOT(prog: BF98Prog, inp: Seq[Char], fip: FIP): FIPRet = fip match{
    case FIP(id, ip, dt, so, bs, stk, binds) => fip.TOSS match{
      case n +: ns =>
        val res = ~n
        FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, dt), dt, so, bs, (res +: ns) +: stk.tail, binds))
    }
  }
  
  def OR(prog: BF98Prog, inp: Seq[Char], fip: FIP): FIPRet = fip match{
    case FIP(id, ip, dt, so, bs, stk, binds) => fip.TOSS match{
      case b +: a +: ns =>
        val res = a | b
        FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, dt), dt, so, bs, (res +: ns) +: stk.tail, binds))
    }
  }
  
  def XOR(prog: BF98Prog, inp: Seq[Char], fip: FIP): FIPRet = fip match{
    case FIP(id, ip, dt, so, bs, stk, binds) => fip.TOSS match{
      case b +: a +: ns =>
        val res = a ^ b
        FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, dt), dt, so, bs, (res +: ns) +: stk.tail, binds))
    }
  }
  
  val binds: Vector[(Char, (BF98Prog, Seq[Char], FIP) => FIPRet)] = Vector(
    'A' -> ((prog: BF98Prog, inp: Seq[Char], fip: FIP) => AND(prog, inp, fip)),
    'N' -> ((prog: BF98Prog, inp: Seq[Char], fip: FIP) => NOT(prog, inp, fip)),
    'O' -> ((prog: BF98Prog, inp: Seq[Char], fip: FIP) => OR(prog, inp, fip)),
    'X' -> ((prog: BF98Prog, inp: Seq[Char], fip: FIP) => XOR(prog, inp, fip)))
}