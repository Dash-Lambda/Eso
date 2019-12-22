package metatape

import common.{Config, Interpreter}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.matching.Regex
import scala.util.{Random, Success, Try}

object Metatape extends Interpreter{
  val name: String = "Metatape"
  
  private val pnam: Regex = raw"""(?s)([^@]*)@([^\{\}/]*)(.*)""".r
  private val namreg1: Regex = raw"""\s*(\S)\s*(.*)\z""".r
  private val namreg2: Regex = raw"""^\s*\{([^\{\}/]*)\}(.*)\z""".r
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = Success{parse(progRaw)} map {
    case (prog, subs) => mtRun(prog, subs, config.rand, config.num("mtCharWidth"))}
  
  def mtRun(prog: Vector[Char], subs: immutable.HashMap[String, Vector[Char]], rand: Random, padLen: Int): Seq[Char] => LazyList[Char] = {
    val initIP = MIP(0, MTape(immutable.HashMap[Int, MCell](), 0, MNull))
    val initCont = RunCont(prog, Vector(), FinCont)
    def initEnv(inputs: Seq[Char]): MEnv = MEnv(rand, bitInp(inputs, padLen), Vector(), padLen, subs)
    
    @tailrec
    def rdo(state: State): Option[(Char, State)] = {
      //Thread.sleep(10)
      //println(s"- State: $state")
      state match{
        case HaltState => None
        case PrintState(c, nxt) => Some((c, nxt))
        case _ => rdo(state.step())}
    }
      
    inputs => LazyList.unfold(RunState(initIP, initEnv(inputs), initCont): State)(rdo)}
  
  def parse(progRaw: String): (Vector[Char], immutable.HashMap[String, Vector[Char]]) = {
    @tailrec
    def pdos(src: String, ac: String = "", mac: immutable.HashMap[String, String] = immutable.HashMap[String, String]()): (Vector[Char], immutable.HashMap[String, Vector[Char]]) = src match{
      case pnam(hd, nm, tl) => blockStr(tl) match{
        case (blk, prg) => pdos(prg, ac ++ hd, mac + ((nm, blk)))}
      case _ => ((ac ++ src).toVector, conditionSubs(mac))}
    
    def blockStr(src: String): (String, String) = {
      @tailrec
      def bdo(i: Int, n: Int = 0): Int = src(i) match{
        case '{' => bdo(i + 1, n + 1)
        case '}' =>
          if(n == 1) i
          else bdo(i + 1, n - 1)
        case _ => bdo(i + 1, n)}
      
      val start = src.indexOf('{')
      val end = bdo(start)
      (src.slice(start + 1, end), src.drop(end + 1))}
    
    def conditionSubs(mac: immutable.HashMap[String, String]): immutable.HashMap[String, Vector[Char]] = mac.map{case (k, v) => (k.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse, v.toVector)}
    
    @tailrec
    def uncomment(src: Vector[Char], ac: Vector[Char] = Vector(), blk: Boolean = false): String = src match{
      case '/' +: '*' +: cs => uncomment(cs, ac, blk=true)
      case '*' +: '/' +: cs if blk => uncomment(cs, ac, blk=false)
      case '/' +: '/' +: cs => uncomment(cs.dropWhile(_ != '\n').drop(1), ac, blk)
      case c +: cs =>
        if(blk) uncomment(cs, ac, blk)
        else uncomment(cs, ac :+ c, blk)
      case _ => ac.mkString}
    
    pdos(uncomment(progRaw.toVector).replaceAll("""[ \t\r\n]+""", " "))}
  
  def bitInp(cinp: Seq[Char], padLen: Int): LazyList[Int] = {
    def binStr(c: Char): Vector[Int] = c.toBinaryString
      .toVector
      .map(_.asDigit)
      .reverse
      .padTo(padLen, 0)
      .take(padLen)
      .reverse
    cinp.to(LazyList).flatMap(binStr)}
  
  trait State{
    def step(): State
  }
  trait Cont{
    def apply(ip: MIP, env: MEnv): State
  }
  
  object HaltState extends State{
    def step(): State = HaltState
  }
  case class RunState(ip: MIP, env: MEnv, cc: Cont) extends State{
    override def toString: String = s"RUNSTATE:\n - IP: $ip\n - CC: $cc"
    def step(): State = cc(ip, env)
  }
  case class PrintState(c: Char, nxt: State) extends State{
    def step(): State = nxt
  }
  
  object FinCont extends Cont{
    override def toString: String = s"FINCONT"
    def apply(ip: MIP, env: MEnv): State = HaltState
  }
  case class SkipCont(prog: Vector[Char], cnt: Int, lcnt: Int, loops: Vector[Vector[Char]], cc: Cont) extends Cont{
    override def toString: String = s"SKIPCONT(${prog.mkString}, $cc)"
    def apply(ip: MIP, env: MEnv): State = prog match{
      case op +: ops => op match{
        case '(' => RunState(ip, env, SkipCont(ops, cnt + 1, lcnt, loops, cc))
        case '|' | ')' if cnt == 0 => RunState(ip, env, RunCont(ops, loops, cc))
        case ')' => RunState(ip, env, SkipCont(ops, cnt - 1, lcnt, loops, cc))
        case '[' => RunState(ip, env, SkipCont(ops, cnt, lcnt + 1, loops, cc))
        case ']' =>
          if(lcnt == 0) RunState(ip, env, SkipCont(ops, cnt, lcnt, loops.tail, cc))
          else RunState(ip, env, SkipCont(ops, cnt, lcnt - 1, loops, cc))
        case _ => RunState(ip, env, SkipCont(ops, cnt, lcnt, loops, cc))}}
  }
  case class ForkCont(sip: MIP, cc: Cont) extends Cont{
    override def toString: String = s"FORKCONT($cc)"
    def apply(ip: MIP, env: MEnv): State = cc(sip.set(ip.cur), env)
  }
  case class BlockCont(loops: Vector[Vector[Char]], cc: Cont) extends Cont{
    def apply(ip: MIP, env: MEnv): State = HaltState
  }
  case class RunCont(prog: Vector[Char], loops: Vector[Vector[Char]], cc: Cont) extends Cont{
    override def toString: String = s"RunCont(${prog.mkString}, $cc)"
    def apply(ip: MIP, env: MEnv): State = prog match{
      case op +: ops => op match{
        case '.' => RunState(ip, env, RunCont(ops, loops, cc))
        case '<' => RunState(ip.moveLeft, env, RunCont(ops, loops, cc))
        case '>' => RunState(ip.moveRight, env, RunCont(ops, loops, cc))
        case 'n' => RunState(ip.set(MNull), env, RunCont(ops, loops, cc))
        case 'e' => RunState(ip.enter, env, RunCont(ops, loops, cc))
        case 'x' => RunState(ip.exit, env, RunCont(ops, loops, cc))
        case '(' =>
          if(ip.curCheck) RunState(ip, env, RunCont(ops, loops, cc))
          else RunState(ip, env, SkipCont(ops, 0, 0, loops, cc))
        case '|' => RunState(ip, env, SkipCont(ops, 0, 0, loops, cc))
        case ')' => RunState(ip, env, RunCont(ops, loops, cc))
        case '[' => RunState(ip, env, RunCont(ops, ops +: loops, cc))
        case ']' => RunState(ip, env, RunCont(loops.head, loops, cc))
        case '{' => RunState(ip, env, RunCont(ops, Vector(), BlockCont(loops, cc)))
        case '}' => cc match{
          case BlockCont(lp2, c2) => RunState(ip, env, RunCont(ops, lp2, c2))}
        case '?' =>
          if(env.rand.nextInt(2) == 0) RunState(ip.set(MNull), env, RunCont(ops, loops, cc))
          else RunState(ip, env, RunCont(ops, loops, cc))
        case 'i' => env.read match{
          case (0, nenv) => RunState(ip.set(MNull), nenv, RunCont(ops, loops, cc))
          case (_, nenv) => RunState(ip, nenv, RunCont(ops, loops, cc))}
        case 'o' => env.write(if(ip.curCheck) 1 else 0) match{
          case (cop, nenv) => cop match{
            case Some(c) => PrintState(c, RunState(ip, nenv, RunCont(ops, loops, cc)))
            case None => RunState(ip, nenv, RunCont(ops, loops, cc))}}
        case '!' => chompName(ops) match{
          case (nam, tl) => RunState(ip, env, RunCont(env.subs(nam), loops, RunCont(tl, loops, cc)))}
        case 'f' => chompFork(ops) match{
          case (frk, prg) => RunState(ip, env, RunCont(frk, loops, ForkCont(ip, RunCont(prg, loops, cc))))}
        case ' ' => RunState(ip, env, RunCont(ops, loops, cc))
        case 'h' => HaltState}
      case _ => RunState(ip, env, cc)}
    
    def chompFork(src: Vector[Char], ac: Vector[Char] = Vector(), cnt: Int = 0): (Vector[Char], Vector[Char]) = src match{
      case c +: cs => c match{
        case '{' =>
          if(cnt == 0) chompFork(cs, ac, cnt + 1)
          else chompFork(cs, ac :+ c, cnt + 1)
        case '}' =>
          if(cnt == 1) (ac, cs)
          else chompFork(cs, ac :+ c, cnt - 1)
        case _ => chompFork(cs, ac :+ c, cnt)}}
    
    def chompName(src: Vector[Char]): (String, Vector[Char]) = src.mkString match{
      case namreg2(nam, tl) => (nam.replaceAll("""\s+""", " "), tl.toVector)
      case namreg1(nam, tl) => (nam.toString, tl.toVector)}
  }
  
  case class MEnv(rand: Random, inp: LazyList[Int], buf: Vector[Int], wid: Int, subs: immutable.HashMap[String, Vector[Char]]){
    def read: (Int, MEnv) = inp match{
      case n +: ns => (n, MEnv(rand, ns, buf, wid, subs))}
    
    def write(n: Int): (Option[Char], MEnv) = {
      val nbuf = buf :+ n
      if(nbuf.sizeIs >= wid) (Some(vecToChar(nbuf)), MEnv(rand, inp, Vector(), wid, subs))
      else (None, MEnv(rand, inp, nbuf, wid, subs))}
    
    def vecToChar(vec: Vector[Int]): Char = BigInt(vec.mkString, 2).toChar
  }
  
  case class MIP(tp: Int, tape: MTape){
    def cur: MCell = tape(tp)
    def curCheck: Boolean = cur != MNull
    
    def set(c: MCell): MIP = MIP(tp, tape.set(tp, c))
    
    def enter: MIP = {
      val ntap = tape.enter(tp)
      MIP(ntap.cur, ntap)}
    def exit: MIP = {
      val ntap = tape.exit(tp)
      MIP(ntap.cur, ntap)}
    
    def moveLeft: MIP = MIP(tp - 1, tape)
    def moveRight: MIP = MIP(tp + 1, tape)
  }
  
  trait MCell{
    def set(c: MCell): MTape
  }
  object MNull extends MCell{
    override def toString: String = "X"
    def init(root: MCell): MTape = MTape(immutable.HashMap[Int, MCell](), 0, root)
    def set(c: MCell): MTape = MTape(immutable.HashMap(0 -> c), 0, MNull)
  }
  case class MTape(tape: immutable.HashMap[Int, MCell], cur: Int, root: MCell) extends MCell{
    override def toString: String = {
      val mini = if(tape.nonEmpty) tape.keys.min else 0
      val maxi = if(tape.nonEmpty) tape.keys.max else 0
      LazyList.range(mini, maxi + 1).map(apply).mkString("[", "", "]")
    }
    def apply(i: Int): MCell = tape.get(i) match{
      case Some(c) => c
      case None => MNull}
    
    def set(c: MCell): MTape = MTape(tape + ((cur, c)), cur, root)
    def set(i: Int, c: MCell): MTape = MTape(tape + ((i, c)), i, root)
    
    def enter(i: Int): MTape = {
      val nrt = MTape(tape, i, root)
      tape.get(i) match{
        case Some(c) => c match{
          case MNull => MNull.init(nrt)
          case MTape(ctp, ccr, _) => MTape(ctp, ccr, nrt)}
        case None => MNull.init(nrt)}}
    def exit(i: Int): MTape = root.set(MTape(tape, i, root))
  }
}
