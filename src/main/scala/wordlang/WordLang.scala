package wordlang

import common.{Config, Interpreter}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.Try

object WordLang extends Interpreter{
  val name: String = "WordLang"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = Try{parse(progRaw)} map{
    case (prog, jumps) =>
      @tailrec
      def rdo(cur: Long, heap: immutable.HashMap[String, Long], mode: Long, ip: Int, inp: Seq[Char]): Option[(String, (Long, immutable.HashMap[String, Long], Long, Int, Seq[Char]))] = {
        prog.lift(ip) match{
          case Some(op) => op match{
            case CharOp(c) => rdo(cur + mode*c.toLong, heap, mode, ip + 1, inp)
            case IncToggle => rdo(cur, heap, -mode, ip + 1, inp)
            case PrintChar => Some((((cur%255 + 255)%255).toChar.toString, (0, heap, mode, ip + 1, inp)))
            case ReadChar => rdo(cur + inp.head, heap, mode, ip + 1, inp.tail)
            case IncByVar(nam) => rdo(cur + mode*heap.getOrElse(nam, 0L), heap, mode, ip + 1, inp)
            case StoreVar(nam) => rdo(0, heap + ((nam, cur)), mode, ip + 1, inp)
            case Jump(nam) => rdo(cur, heap, mode, jumps(nam), inp)
            case JumpLess(nam) =>
              if(cur < 0) rdo(cur, heap, mode, jumps(nam), inp)
              else rdo(cur, heap, mode, ip + 1, inp)
            case JumpGreater(nam) =>
              if(cur > 0) rdo(cur, heap, mode, jumps(nam), inp)
              else rdo(cur, heap, mode, ip + 1, inp)
            case PrintNumOp => Some((cur.toString, (0, heap, mode, ip + 1, inp)))
            case DebugOp if config.bool("debug") => Some((s"\n[ip=$ip, data=$cur (${cur.toChar})]\n", (cur, heap, mode, ip + 1, inp)))
            case _ => rdo(cur, heap, mode, ip + 1, inp)}
          case _ => None}}
      inputs => LazyList.unfold((0: Long, immutable.HashMap[String, Long](), 1L, 0, inputs)){
        case (cur, heap, mode, src, inp) => rdo(cur, heap, mode, src, inp)}.flatten}
  
  def parse(progRaw: String): (Vector[WOP], immutable.HashMap[String, Int]) = {
    @tailrec
    def pdo(src: Vector[Char], ac: Vector[WOP], acm: immutable.HashMap[String, Int], sv: Boolean, rv: Boolean, bp: Boolean, ct: String): (Vector[WOP], immutable.HashMap[String, Int]) = src match{
      case op +: ops => op match{
        case ' ' | '\t' | '\n' | '\r' =>
          if(sv && ct.nonEmpty) pdo(ops, ac :+ StoreVar(ct), acm, sv=false, rv, bp, "")
          else if(bp && ct.nonEmpty) pdo(ops, ac, acm + ((ct, ac.size)), sv, rv, bp=false, "")
          else pdo(ops, ac, acm, sv, rv, bp, ct)
        case '\'' =>
          if(rv) pdo(ops, ac :+ IncByVar(ct), acm, sv, !rv, bp, "")
          else pdo(ops, ac, acm, sv, !rv, bp, "")
        case '"' => pdo(ops.dropWhile(_ != '"').drop(1), ac, acm, sv, rv, bp, "")
        case '>' => pdo(ops, ac, acm, sv=true, rv, bp, "")
        case '-' => pdo(ops, ac, acm, sv, rv, bp=true, "")
        case '!' =>
          val num = math.min(ops.takeWhile(_ == '!').size, 2)
          val nop = num match{
            case 0 => Jump(ct)
            case 1 => JumpLess(ct)
            case 2 => JumpGreater(ct)}
          pdo(ops.drop(num), ac.dropRight(ct.length) :+ nop, acm, sv, rv, bp, "")
        case '\\' if !(sv || rv || bp) => pdo(ops.tail, ac :+ CharOp(ops.head), acm, sv, rv, bp, ct + ops.head)
        case '(' => pdo(ops, ac, acm, sv, rv, bp, ct)
        case '?' => pdo(ops, ac :+ DebugOp, acm, sv, rv, bp, "")
        case ',' => pdo(ops, ac :+ IncToggle, acm, sv, rv, bp, "")
        case '.' => pdo(ops, ac :+ PrintChar, acm, sv, rv, bp, "")
        case '<' => pdo(ops, ac :+ ReadChar, acm, sv, rv, bp, "")
        case ')' => pdo(ops, ac :+ PrintNumOp, acm, sv, rv, bp, ct)
        case _ =>
          if(sv || rv || bp) pdo(ops, ac, acm, sv, rv, bp, ct + op)
          else pdo(ops, ac :+ CharOp(op), acm, sv, rv, bp, ct + op)}
      case _ => (ac, acm)}
    pdo(progRaw.toVector, Vector(), immutable.HashMap(), sv=false, rv=false, bp=false, "")}
  
  trait WOP
  case class CharOp(c: Char) extends WOP
  object IncToggle extends WOP
  object PrintChar extends WOP
  object ReadChar extends WOP
  case class IncByVar(name: String) extends WOP
  case class StoreVar(name: String) extends WOP
  case class JumpLabel(name: String) extends WOP
  case class Jump(name: String) extends WOP
  case class JumpLess(name: String) extends WOP
  case class JumpGreater(name: String) extends WOP
  object PrintNumOp extends WOP
  object DebugOp extends WOP
}
