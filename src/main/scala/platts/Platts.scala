package platts

import common.{Config, Interpreter}
import parsers.{EsoParseFail, EsoParsed, RegexParser}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.Try

object Platts extends Interpreter{
  val name: String = "Platts"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = Try{parse(progRaw)} map{
    case (symLen, ruleMap, initData) =>
      val stepper = RegexParser(raw"""(.{$symLen}).{$symLen}"""){ m => ruleMap.getOrElse(m.group(1), REP("", toggle=false))}
      
      @tailrec
      def repInp(ac: String, inp: Seq[Char]): (String, Seq[Char]) = {
        if(!ac.contains(-1.toChar)) (ac, inp)
        else inp match{
          case c +: cs => repInp(ac.replaceFirst(s"${-1.toChar}", c.toString), cs)}}
      
      @tailrec
      def rdo(ac: String, out: Boolean, inp: Seq[Char]): Option[(String, (String, Boolean, Seq[Char]))] = stepper(ac) match{
        case EsoParseFail => None
        case EsoParsed(op, tl, _, _) =>
          op match{
            case Halt => None
            case REP(str, toggle) => repInp(str, inp) match{
              case (prod, ninp) =>
                val nout = if(toggle) !out else out
                if(nout) Some((prod, (tl ++ prod, nout, ninp)))
                else rdo(tl ++ prod, nout, ninp)}}}
      
      inputs => LazyList.unfold((initData, false, inputs)){case (ac, out, inp) => rdo(ac, out, inp)}.flatten}
  
  def parse(progRaw: String): (Int, immutable.HashMap[String, POP], String) = {
    val lines = progRaw.replaceAllLiterally("{input}", (-1).toChar.toString).linesIterator.to(LazyList)
    val num = lines.head.toInt
    val rules = lines.tail.take(num)
    val initial = lines.drop(num + 1)
    
    val symLen = Iterator.range(0, rules.head.length).find{i => rules.forall(s => "|>!".contains(s(i)))}.get
    
    val opReg = raw"""(.{$symLen})([\!\>\|])(.*)""".r
    val rulesParsed = rules map{
      case opReg(t, s, tl) =>
        val op = s match{
          case "|" => REP(tl, toggle=false)
          case ">" => REP(tl, toggle=true)
          case "!" => Halt}
        (t, op)}
    
    val ruleMap = mkMap(rulesParsed)
    val initData = initial.mkString
    (symLen, ruleMap, initData)}
  
  trait POP
  object Halt extends POP
  case class REP(str: String, toggle: Boolean) extends POP
}
