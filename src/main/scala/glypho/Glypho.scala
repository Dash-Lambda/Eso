package glypho

import common.{Config, ContinuationInterpreter, ICont, IHaltCont, IHaltState, IPrintState, IState, Interpreter}

import scala.util.Try

object Glypho extends Interpreter{
  val name: String = "Glypho"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    Try{GlyphoParser.parseAll(progRaw)} map{initProg =>
      inputs => {
        val initState: IState = RunState(GEnv(Vector(), inputs), RunCont(initProg, Vector(), IHaltCont(GEnv(Vector(), Vector()))))
        ContinuationInterpreter(initState)}}}
  
  case class GEnv(stk: Vector[Int], inp: Seq[Char]){
    def pop: (Int, GEnv) = stk match{
      case n +: ns => (n, GEnv(ns, inp))}
    def pop(num: Int): (Vector[Int], GEnv) = (stk.take(num), GEnv(stk.drop(num), inp))
    def push(ns: Int*): GEnv = GEnv(ns ++: stk, inp)
    def pushBack(ns: Int*): GEnv = GEnv(stk :++ ns.reverse, inp)
    
    def rollForward: GEnv = stk match{
      case n +: ns => GEnv(ns :+ n, inp)}
    def rollBack: GEnv = stk match{
      case ns :+ n => GEnv(n +: ns, inp)}
    
    def read: (Char, GEnv) = inp match{
      case c +: cs => (c, GEnv(stk, cs))}
    
    def binOp(f: (Int, Int) => Seq[Int]): GEnv = stk match{
      case a +: b +: ns => GEnv(f(a, b) ++: ns, inp)}
    def binOp1(f: (Int, Int) => Int): GEnv = stk match{
      case a +: b +: ns => GEnv(f(a, b) +: ns, inp)}
    def singOp(f: Int => Int): GEnv = stk match{
      case n +: ns => GEnv(f(n) +: ns, inp)}}
  
  case class RunState(env: GEnv, cc: ICont[GEnv]) extends IState{
    def next: IState = cc(env)}
  
  case class SkipCont(i: Int, prog: Vector[Char], loops: Vector[Vector[Char]], cc: ICont[GEnv]) extends ICont[GEnv]{
    def apply(env: GEnv): IState = prog match{
      case c +: cs => c match{
        case '[' => RunState(env, SkipCont(i + 1, cs, loops, cc))
        case ']' =>
          if(i == 0) RunState(env, RunCont(cs, loops, cc))
          else RunState(env, SkipCont(i - 1, cs, loops, cc))
        case _ => RunState(env, SkipCont(i, cs, loops, cc))}}}
  
  case class RunCont(prog: Vector[Char], loops: Vector[Vector[Char]], cc: ICont[GEnv]) extends ICont[GEnv]{
    def apply(env: GEnv): IState = prog match{
      case c +: cs =>
        lazy val nxc = RunCont(cs, loops, cc)
        c match{
          case 'n' => RunState(env, nxc)
          case 'i' => env.read match{
              case (c, nenv) => RunState(nenv.push(c.toInt), nxc)}
          case '>' => RunState(env.rollForward, nxc)
          case '\\' => env.pop(2) match{
              case (a +: b +: _, nenv) => RunState(nenv.push(b, a), nxc)}
          case '1' => RunState(env.push(1), nxc)
          case '<' => RunState(env.rollBack, nxc)
          case 'd' => RunState(env.push(env.stk.head), nxc)
          case '+' => RunState(env.binOp1(_+_), nxc)
          case '[' =>
            if(env.stk.head == 0) RunState(env, SkipCont(0, cs, loops, cc))
            else RunState(env, RunCont(cs, cs +: loops, cc))
          case 'o' => env.pop match{
              case (n, nenv) => IPrintState(n.toChar.toString, RunState(nenv, nxc))}
          case '*' => RunState(env.binOp1(_*_), nxc)
          case 'e' => env.pop(4) match{
            case (a +: b +: c +: d +: _, nenv) =>
              val nop = GlyphoParser.parseOne(Vector(a, b, c, d).map(_.toChar).mkString)
              RunState(nenv, RunCont(nop +: cs, loops, cc))}
          case '-' => RunState(env.singOp(-_), nxc)
          case '!' => RunState(env.pop._2, nxc)
          case ']' =>
            if(env.stk.head == 0) RunState(env, RunCont(cs, loops.tail, cc))
            else RunState(env, RunCont(loops.head, loops, cc))
          case _ => RunState(env, nxc)}
      case _ => IHaltState}}
}
