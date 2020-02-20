package metatape

import common.{Config, Interpreter}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.matching.Regex
import scala.util.Try

import spire.implicits._

object Metatape extends Interpreter{
  val name: String = "Metatape"
  val comReg: Regex = raw"""(?m)(?s)(?://[^\n]*$$|/\*.*\*/)""".r
  val namReg: Regex = raw"""^ ?((?:\S|\S.*\S)) ?\z""".r
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = Try{parse(progRaw)} map{
    case (prog, subs) => runProg(prog, subs, config.rands, config.num("charWidth"))}
  
  def runProg(prog: Vector[MTOP], subVec: Vector[MSub], rands: LazyList[Int], padLen: Int): Seq[Char] => LazyList[Char] = {
    val subs = mkMap(subVec.map{case MSub(nam, sub) => (nam, sub)})
    val initIP = MDP(0, MTape(None, None, Vector(), Vector()))
    @tailrec
    def rdo(state: State): Option[(Char, State)] = state match{
      case HaltState => None
      case PrintState(c, nxt) => Some((c, nxt))
      case _ => rdo(state.step())}
    
    inputs => {
      val initEnv = MEnv(rands, bitInp(inputs, padLen), Vector(), padLen, subs)
      LazyList.unfold(RunState(initIP, initEnv, RunCont(0, prog, FinCont)): State)(rdo)}}
  
  def bitInp(cinp: Seq[Char], padLen: Int): LazyList[Int] = {
    def binStr(c: Char): Vector[Int] = c.toBinaryString
      .toVector
      .map(_.asDigit)
      .reverse
      .padTo(padLen, 0)
      .take(padLen)
      .reverse
    cinp.to(LazyList).flatMap(binStr)}
  
  trait MTOP
  case class MMove(n: Int) extends MTOP
  case class MEnter(n: Int) extends MTOP
  case class MExit(n: Int) extends MTOP
  object MSetNull extends MTOP
  case class MRand(n: Int) extends MTOP
  case class MIf(ind: Int) extends MTOP
  case class MElse(ind: Int) extends MTOP
  case class MLoop(ind: Int) extends MTOP
  object MInp extends MTOP
  object MOut extends MTOP
  object MHalt extends MTOP
  case class MCall(nam: String) extends MTOP
  case class MFork(prog: Vector[MTOP]) extends MTOP
  case class MBlock(prog: Vector[MTOP]) extends MTOP
  
  trait State{
    def step(): State
  }
  trait Cont{
    def apply(ip: MDP, env: MEnv): State
  }
  
  object HaltState extends State{
    def step(): State = HaltState
  }
  case class RunState(tape: MDP, env: MEnv, cc: Cont) extends State{
    def step(): State = cc(tape, env)
  }
  case class PrintState(c: Char, nxt: State) extends State{
    def step(): State = nxt
  }
  
  object FinCont extends Cont{
    def apply(ip: MDP, env: MEnv): State = HaltState
  }
  case class ForkCont(saveTape: MDP, cc: Cont) extends Cont{
    def apply(ip: MDP, env: MEnv): State = cc(saveTape.set(ip.cur), env)
  }
  case class RunCont(i: Int, prog: Vector[MTOP], cc: Cont) extends Cont{
    def apply(tape: MDP, env: MEnv): State = prog.lift(i) match{
      case None => RunState(tape, env, cc)
      case Some(op) => op match{
        case MMove(n) => RunState(tape.move(n), env, RunCont(i + 1, prog, cc))
        case MEnter(n) => RunState(tape.enter(n), env, RunCont(i + 1, prog, cc))
        case MExit(n) => RunState(tape.exit(n), env, RunCont(i + 1, prog, cc))
        case MSetNull => RunState(tape.set(None), env, RunCont(i + 1, prog, cc))
        case MRand(n) => env.randInt match{
          case (r, nenv) => RunState(if(r%n == 0) tape.set(None) else tape, nenv, RunCont(i + 1, prog, cc))}
        case MIf(ind) => RunState(tape, env, RunCont(if(tape.curCheck) i + 1 else ind, prog, cc))
        case MElse(ind) => RunState(tape, env, RunCont(ind, prog, cc))
        case MLoop(ind) => RunState(tape, env, RunCont(ind, prog, cc))
        case MHalt => HaltState
        case MCall(nam: String) => RunState(tape, env, RunCont(0, env.subs(nam), RunCont(i + 1, prog, cc)))
        case MFork(blk) => RunState(tape, env, RunCont(0, blk, ForkCont(tape, RunCont(i + 1, prog, cc))))
        case MBlock(blk) => RunState(tape, env, RunCont(0, blk, RunCont(i + 1, prog, cc)))
        case MInp => env.read match{
          case (0, nenv) => RunState(tape.set(None), nenv, RunCont(i + 1, prog, cc))
          case (_, nenv) => RunState(tape, nenv, RunCont(i + 1, prog, cc))}
        case MOut => env.write(if(tape.curCheck) 1 else 0) match{
          case (cop, nenv) => cop match{
            case Some(c) => PrintState(c, RunState(tape, nenv, RunCont(i + 1, prog, cc)))
            case None => RunState(tape, nenv, RunCont(i + 1, prog, cc))}}}}
  }
  
  
  case class MSub(nam: String, prog: Vector[MTOP])
  
  def parse(progRaw: String): (Vector[MTOP], Vector[MSub]) = {
    val prog = comReg.replaceAllIn(progRaw, "").replaceAll("""\s+""", " ").toVector
    val oneMap = immutable.HashMap(
      '>' -> MMove(1),
      '<' -> MMove(-1),
      'e' -> MEnter(1),
      'x' -> MExit(1),
      'n' -> MSetNull,
      '?' -> MRand(1),
      'i' -> MInp,
      'o' -> MOut,
      'h' -> MHalt)
    
    def setJump(ac: Vector[MTOP], i: Int, j: Int): Vector[MTOP] = ac(i) match{
      case MIf(_) => ac.updated(i, MIf(j))
      case MElse(_) => ac.updated(i, MElse(j))}
    def subName(ind: Int, call: Boolean = false): (String, Int) = {
      @tailrec
      def cdo(i: Int, ac: Vector[Char] = Vector()): (String, Int) = prog(i) match{
        case '}' | '{' => ac.mkString match{
          case namReg(nam) => (nam, i + 1)}
        case c => cdo(i + 1, ac :+ c)}
      prog(ind) match{
        case '{' => cdo(ind + 1)
        case c if call => (c.toString, ind + 1)
        case _ => cdo(ind)}}
    def chompSub(ind: Int): (MSub, Int) = subName(ind) match{
      case (nam, i1) => odoRecur(i1) match{
        case (blk, _, i2) => (MSub(nam, blk), i2)}}
    
    def odoRecur(ind: Int): (Vector[MTOP], Vector[MSub], Int) = odo(i = ind)
    @tailrec
    def odo(i: Int = 0, j: Int = 0, n: Int = 0, cur: Char = ' ', ac: Vector[MTOP] = Vector(), lstk: Vector[Int] = Vector(), istk: Vector[Int] = Vector(), subs: Vector[MSub] = Vector()): (Vector[MTOP], Vector[MSub], Int) = {
      lazy val op = cur match{
        case '>' => MMove(n)
        case '<' => MMove(-n)
        case 'e' => MEnter(n)
        case 'x' => MExit(n)
        case '?' => MRand(2 ** n)}
      lazy val nxtAc = if(n == 0) ac else ac :+ op
      lazy val jinc = if(n == 0) j else j + 1
      prog.lift(i) match{
        case None => (nxtAc, subs, i)
        case Some(c) =>
          c match{
            case '.' | ' ' => odo(i + 1, j, n, cur, ac, lstk, istk, subs)
            case 'n' if cur == c => odo(i + 1, j, 0, c, ac, lstk, istk, subs)
            case '(' => odo(i + 1, jinc + 1, 0, c, nxtAc :+ MIf(0), lstk, jinc +: istk, subs)
            case '|' => istk match{
              case k +: ks => odo(i + 1, jinc + 1, 0, c, setJump(nxtAc, k, jinc + 1) :+ MElse(0), lstk, jinc +: ks, subs)
              case _ => odo(i + 1, jinc + 1, 0, c, nxtAc :+ MElse(0), lstk, jinc +: istk, subs)}
            case ')' => odo(i + 1, jinc, 0, c, setJump(nxtAc, istk.head, jinc), lstk, istk.tail, subs)
            case '[' => odo(i + 1, jinc, 0, c, nxtAc, jinc +: lstk, istk, subs)
            case ']' => odo(i + 1, jinc + 1, 0, c, nxtAc :+ MLoop(lstk.head), lstk.tail, istk, subs)
            case '{' => odoRecur(i + 1) match{
              case (blk, nsubs, ni) => odo(ni, jinc + 1, 0, c, nxtAc :+ MBlock(blk), lstk, istk, subs ++ nsubs)}
            case '}' => (nxtAc, subs, i + 1)
            case 'f' => prog(i + 1) match{
              case '{' => odoRecur(i + 2) match{
                case (blk, nsubs, ni) => odo(ni, jinc + 1, 0, c, nxtAc :+ MFork(blk), lstk, istk, subs ++ nsubs)}
              case c2 => odo(i + 2, jinc + 1, 0, c, nxtAc :+ MFork(Vector(oneMap(c2))), lstk, istk, subs)}
            case '!' => subName(i + 1, call = true) match{
              case (nam, ni) => odo(ni, jinc + 1, 0, c, nxtAc :+ MCall(nam), lstk, istk, subs)}
            case '@' => chompSub(i + 1) match{
              case (sub, ni) => odo(ni, jinc, 0, c, nxtAc, lstk, istk, subs :+ sub)}
            case 'h' | 'i' | 'o' | 'n' => odo(i + 1, jinc + 1, 0, c, nxtAc :+ oneMap(c), lstk, istk, subs)
            case _ =>
              if(c == cur) odo(i + 1, j, n + 1, cur, ac, lstk, istk, subs)
              else if(n == 0) odo(i + 1, j, 1, c, ac, lstk, istk, subs)
              else odo(i + 1, j + 1, 1, c, ac :+ op, lstk, istk, subs)}}}
    
    odo() match{
      case (ops, subs, _) => (ops, subs)}}
  
  case class MEnv(rands: LazyList[Int], inp: LazyList[Int], buf: Vector[Int], wid: Int, subs: immutable.HashMap[String, Vector[MTOP]]){
    def read: (Int, MEnv) = inp match{
      case n +: ns => (n, MEnv(rands, ns, buf, wid, subs))}
    
    def write(n: Int): (Option[Char], MEnv) = {
      val nbuf = buf :+ n
      if(nbuf.sizeIs >= wid) (Some(vecToChar(nbuf)), MEnv(rands, inp, Vector(), wid, subs))
      else (None, MEnv(rands, inp, nbuf, wid, subs))}
    
    def vecToChar(vec: Vector[Int]): Char = BigInt(vec.mkString, 2).toChar
    
    def randInt: (Int, MEnv) = rands match{
      case r +: rs => (r, MEnv(rs, inp, buf, wid, subs))}
  }
  
  case class MDP(dp: Int, tape: MTape){
    def cur: Option[MCell] = tape(dp)
    def curCheck: Boolean = cur.isDefined
    
    def set(c: Option[MCell]): MDP = MDP(dp, tape.set(dp, c))
    
    def enter(num: Int): MDP = {
      @tailrec
      def edo(n: Int, t: MTape): MTape = {
        if(n == 0) t
        else edo(n - 1, t.enter)}
      MDP(0, edo(num, tape.move(dp)))}
    
    def exit(num: Int): MDP = {
      @tailrec
      def edo(n: Int, t: MTape): MTape = {
        if(n == 0) t
        else edo(n - 1, t.exit)}
      MDP(0, edo(num, tape.move(dp)))}
    
    def move(n: Int): MDP = MDP(dp + n, tape)
  }
  
  case class MCell(child: Option[MCell], left: Vector[Option[MCell]], right: Vector[Option[MCell]])
  case class MTape(parent: Option[MCell], child: Option[MCell], left: Vector[Option[MCell]], right: Vector[Option[MCell]]){
    def apply(i: Int): Option[MCell] = {
      if(i > 0) right.lift(i - 1).flatten
      else if(i < 0) left.lift(-i - 1).flatten
      else child}
    
    def set(i: Int, c: Option[MCell]): MTape = {
      if(i > 0){
        if(right.isDefinedAt(i - 1)) MTape(parent, child, left, right.updated(i - 1, c))
        else MTape(parent, child, left, right.padTo(i - 1, None) :+ c)}
      else if(i < 0){
        if(left.isDefinedAt(-i - 1)) MTape(parent, child, left.updated(-i - 1, c), right)
        else MTape(parent, child, left.padTo(-i, None) :+ c, right)}
      else MTape(parent, c, left, right)}
    
    def move(i: Int): MTape = {
      if(i > 0){
        if(right.isDefinedAt(i - 1)) MTape(parent, right(i - 1), right.take(i - 1).reverse ++: child +: left, right.drop(i))
        else MTape(parent, None, right.padTo(i - 1, None).reverse ++: child +: left, Vector())}
      else if(i < 0){
        if(left.isDefinedAt(-i - 1)) MTape(parent, left(-i - 1), left.drop(-i), left.take(-i - 1).reverse ++: child +: right)
        else MTape(parent, None, Vector(), left.padTo(-i - 1, None).reverse ++: child +: right)}
      else this}
    
    def enter: MTape = {
      val nparent = {
        if(parent.isDefined || left.nonEmpty || right.nonEmpty) Some(MCell(parent, left, right))
        else None}
      child match{
        case Some(MCell(nchild, nleft, nright)) => MTape(nparent, nchild, nleft, nright)
        case _ => MTape(nparent, None, Vector(), Vector())}}
    def exit: MTape = parent match{
      case Some(MCell(nparent, nleft, nright)) => MTape(nparent, Some(MCell(child, left, right)), nleft, nright)
      case None => MTape(None, Some(MCell(child, left, right)), Vector(), Vector())}
  }
}
