package glypho

import common.{Config, Interpreter, PartialParser, RegexParser}

import scala.annotation.tailrec
import scala.util.Try

object Glypho extends Interpreter{
  val name: String = "Glypho"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    trait State{
      def next: State}
    trait Cont{
      def apply(stk: Vector[Int], inp: Seq[Char]): State}
    
    object HaltState extends State{
      def next: State = HaltState}
    case class OutState(c: Char, next: State) extends State
    case class RunState(stk: Vector[Int], inp: Seq[Char], cc: Cont) extends State{
      def next: State = cc(stk, inp)}
    
    object HaltCont extends Cont{
      def apply(stk: Vector[Int], inp: Seq[Char]): State = HaltState}
    case class SkipCont(i: Int, prog: Vector[Char], loops: Vector[Vector[Char]], cc: Cont) extends Cont{
      def apply(stk: Vector[Int], inp: Seq[Char]): State = prog match{
        case c +: cs => c match{
          case '[' => RunState(stk, inp, SkipCont(i + 1, cs, loops, cc))
          case ']' =>
            if(i == 0) RunState(stk, inp, RunCont(cs, loops, cc))
            else RunState(stk, inp, SkipCont(i - 1, cs, loops, cc))
          case _ => RunState(stk, inp, SkipCont(i, cs, loops, cc))}}}
    case class RunCont(prog: Vector[Char], loops: Vector[Vector[Char]], cc: Cont) extends Cont{
      def apply(stk: Vector[Int], inp: Seq[Char]): State = prog match{
        case c +: cs =>
          lazy val nxc = RunCont(cs, loops, cc)
          c match{
            case 'n' => RunState(stk, inp, nxc)
            case 'i' => RunState(inp.head.toInt +: stk, inp.tail, nxc)
            case '>' => RunState(stk.tail :+ stk.head, inp, nxc)
            case '\\' => stk match{
              case a +: b +: ns => RunState(b +: a +: ns, inp, nxc)}
            case '1' => RunState(1 +: stk, inp, nxc)
            case '<' => RunState(stk.last +: stk.init, inp, nxc)
            case 'd' => RunState(stk.head +: stk, inp, nxc)
            case '+' => stk match{
              case a +: b +: ns => RunState((a + b) +: ns, inp, nxc)}
            case '[' =>
              if(stk.head == 0) RunState(stk, inp, SkipCont(0, cs, loops, cc))
              else RunState(stk, inp, RunCont(cs, cs +: loops, cc))
            case 'o' => OutState(stk.head.toChar, RunState(stk.tail, inp, nxc))
            case '*' => stk match{
              case a +: b +: ns => RunState((a*b) +: ns, inp, nxc)}
            case 'e' => stk match{
              case a +: b +: c +: d +: ns =>
                val nop = parseOne(Vector(a, b, c, d).map(_.toChar).mkString)
                RunState(ns, inp, RunCont(nop +: cs, loops, cc))}
            case '-' => RunState((-stk.head) +: stk.tail, inp, nxc)
            case '!' => RunState(stk.tail, inp, nxc)
            case ']' =>
              if(stk.head == 0) RunState(stk, inp, RunCont(cs, loops.tail, cc))
              else RunState(stk, inp, RunCont(loops.head, loops, cc))
            case _ => RunState(stk, inp, nxc)}
        case _ => HaltState}}
    
    @tailrec
    def rdo(state: State): Option[(Char, State)] = state.next match{
      case OutState(c, nxt) => Some((c, nxt))
      case HaltState => None
      case nxt => rdo(nxt)}
    
    Try{parse(progRaw)} map{initProg =>
      def initState(inp: Seq[Char]): State = RunState(Vector(), inp, RunCont(initProg, Vector(), HaltCont))
      inputs => LazyList.unfold(initState(inputs))(rdo)}}
  
  def parse(progRaw: String): Vector[Char] = {
    val normParse = RegexParser[Char](raw"""(....)"""){m => parseOne(m.group(1))}
    normParse.parseAll(progRaw)}
  
  def parseOne(tok: String): Char = {
    @tailrec
    def normalize(src: Seq[Char], ac: Vector[Int] = Vector(), maps: Vector[Char] = Vector()): String = src match{
      case c +: cs => maps.indexOf(c) match{
        case -1 => normalize(cs, ac :+ maps.size, maps :+ c)
        case n => normalize(cs, ac :+ n, maps)}
      case _ => ac.mkString}
    def toOp(str: String): Char = str match{
      case "0000" => 'n'
      case "0001" => 'i'
      case "0010" => '>'
      case "0011" => '\\'
      case "0012" => '1'
      case "0100" => '<'
      case "0101" => 'd'
      case "0102" => '+'
      case "0110" => '['
      case "0111" => 'o'
      case "0112" => '*'
      case "0120" => 'e'
      case "0121" => '-'
      case "0122" => '!'
      case "0123" => ']'}
    toOp(normalize(tok))}
}
