package glypho

import common.{Config, Interpreter}

import scala.annotation.tailrec
import scala.util.Try

object Glypho extends Interpreter{
  val name: String = "Glypho"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    @tailrec
    def rdo(state: State): Option[(Char, State)] = state.next match{
      case OutState(c, nxt) => Some((c, nxt))
      case HaltState => None
      case nxt => rdo(nxt)}
    
    Try{GlyphoParser.parseAll(progRaw)} map{initProg =>
      inputs => {
        val initState: State = RunState(Vector(), inputs, RunCont(initProg, Vector(), HaltCont))
        LazyList.unfold(initState)(rdo)}}}
  
  trait State{
    def next: State}
  
  object HaltState extends State{
    def next: State = HaltState}
  
  case class OutState(c: Char, next: State) extends State
  
  case class RunState(stk: Vector[Int], inp: Seq[Char], cc: Cont) extends State{
    def next: State = cc(stk, inp)}
  
  trait Cont{
    def apply(stk: Vector[Int], inp: Seq[Char]): State}
  
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
              val nop = GlyphoParser.parseOne(Vector(a, b, c, d).map(_.toChar).mkString)
              RunState(ns, inp, RunCont(nop +: cs, loops, cc))}
          case '-' => RunState((-stk.head) +: stk.tail, inp, nxc)
          case '!' => RunState(stk.tail, inp, nxc)
          case ']' =>
            if(stk.head == 0) RunState(stk, inp, RunCont(cs, loops.tail, cc))
            else RunState(stk, inp, RunCont(loops.head, loops, cc))
          case _ => RunState(stk, inp, nxc)}
      case _ => HaltState}}
}
