package wordlang

import common.{Config, Interpreter, OrderedParser, OrderedRegexParser}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.Try
import scala.util.matching.Regex

object WordLang extends Interpreter{
  val name: String = "WordLang"
  
  val escapeReg: Regex = raw"""\\(.)""".r
  val commentReg: Regex = raw"""(?s)"[^"]*"""".r
  val wordLangParser: OrderedParser[String, WOP] = {
    def flipBack(str: String): String = str.map(_.abs)
    val ibvParser = OrderedRegexParser[WOP](raw"""'(\S*)'""")(m => IncByVar(flipBack(m.group(1))))
    val ivbParser = OrderedRegexParser[WOP](raw"""\>\s*(\S*)""")(m => IncVarBy(flipBack(m.group(1))))
    val lblParser = OrderedRegexParser[WOP](raw"""\-\s*(\w*)""")(m => JumpLabel(flipBack(m.group(1))))
    val jmpParser = OrderedRegexParser[WOP](raw"""(\w*)(\!{1,3})"""){m =>
      m.group(2).length match{
        case 1 => Jump(flipBack(m.group(1)))
        case 2 => JumpLess(flipBack(m.group(1)))
        case 3 => JumpGreater(flipBack(m.group(1)))}}
    val charParser = OrderedRegexParser[WOP](raw"""(\S)"""){m =>
      m.group(1).head match{
        case ',' => IncToggle
        case '.' => PrintChar
        case '<' => ReadChar
        case '?' => DebugOp
        case c => CharOp(c.abs)}}
    
    val baseParser = ibvParser <+> ivbParser <+> lblParser <+> jmpParser <+> charParser
    val parParser = OrderedRegexParser[WOP](raw"""\(([^\(\)]*)\)""")(m => PrintNum(baseParser.parseAllValues(m.group(1))))
    baseParser <+> parParser}
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = Try{parse(progRaw)} map{
    case (prog, jumps) =>
      @tailrec
      def rdo(cur: Long, heap: immutable.HashMap[String, Long], mode: Boolean, src: Vector[Vector[WOP]], inp: Seq[Char]): Option[(String, (Long, immutable.HashMap[String, Long], Boolean, Vector[Vector[WOP]], Seq[Char]))] = {
        src match{
          case hd +: tl => hd match{
            case op +: ops => op match{
              case CharOp(c) =>
                if(mode) rdo(cur + c.toLong, heap, mode, ops +: tl, inp)
                else rdo(cur - c.toLong, heap, mode, ops +: tl, inp)
              case IncToggle => rdo(cur, heap, !mode, ops +: tl, inp)
              case PrintChar => Some((((cur%255 + 255)%255).toChar.toString, (0, heap, mode, ops +: tl, inp)))
              case ReadChar => rdo(cur + inp.head, heap, mode, ops +: tl, inp.tail)
              case IncByVar(nam) =>
                if(mode) rdo(cur + heap.getOrElse(nam, 0L), heap, mode, ops +: tl, inp)
                else rdo(cur - heap.getOrElse(nam, 0L), heap, mode, ops +: tl, inp)
              case IncVarBy(nam) => rdo(0, heap + ((nam, cur)), mode, ops +: tl, inp)
              case Jump(nam) => rdo(cur, heap, mode, prog.drop(jumps(nam)) +: tl, inp)
              case JumpLess(nam) =>
                if(cur < 0) rdo(cur, heap, mode, prog.drop(jumps(nam)) +: tl, inp)
                else rdo(cur, heap, mode, ops +: tl, inp)
              case JumpGreater(nam) =>
                if(cur > 0) rdo(cur, heap, mode, prog.drop(jumps(nam)) +: tl, inp)
                else rdo(cur, heap, mode, ops +: tl, inp)
              case PrintNum(code) => rdo(cur, heap, mode, (code :+ PrintNumOp) +: ops +: tl, inp)
              case PrintNumOp => Some((cur.toString, (0, heap, mode, tl, inp)))
              case DebugOp => rdo(cur, heap, mode, ops +: tl, inp)}
            case _ => rdo(cur, heap, mode, tl, inp)}
          case _ => None}}
      inputs => LazyList.unfold((0: Long, immutable.HashMap[String, Long](), true, Vector(prog), inputs)){
        case (cur, heap, mode, src, inp) => rdo(cur, heap, mode, src, inp)}.flatten}
  
  def parse(progRaw: String): (Vector[WOP], immutable.HashMap[String, Int]) = {
    val escaped = escapeReg
      .replaceAllIn(progRaw, m => (-m.group(1).head).toChar.toString)
    val uncommented = commentReg
      .replaceAllIn(escaped, "")
      .linesIterator
      .mkString(" ")
    
    @tailrec
    def pdo(src: Seq[WOP], ac: Vector[WOP], acm: immutable.HashMap[String, Int]): (Vector[WOP], immutable.HashMap[String, Int]) = src match{
      case op +: ops => op match{
        case JumpLabel(nam) => pdo(ops, ac, acm + ((nam, ac.size)))
        case _ => pdo(ops, ac :+ op, acm)}
      case _ => (ac, acm)}
    pdo(wordLangParser.parseAllValuesLazy(uncommented), Vector(), immutable.HashMap())}
  
  trait WOP
  case class CharOp(c: Char) extends WOP
  object IncToggle extends WOP
  object PrintChar extends WOP
  object ReadChar extends WOP
  case class IncByVar(name: String) extends WOP
  case class IncVarBy(name: String) extends WOP
  case class JumpLabel(name: String) extends WOP
  case class Jump(name: String) extends WOP
  case class JumpLess(name: String) extends WOP
  case class JumpGreater(name: String) extends WOP
  case class PrintNum(code: Vector[WOP]) extends WOP
  object PrintNumOp extends WOP
  object DebugOp extends WOP
}
