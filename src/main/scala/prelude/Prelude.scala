package prelude

import common.{Config, Interpreter, Matrix}
import spire.math.SafeLong
import spire.implicits._

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.parallel.immutable.ParVector
import scala.util.{Success, Try}

object Prelude extends Interpreter{
  val name: String = "Prelude"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = parse(progRaw) map pRun(config.bool("preludePar"))
  
  def pRun(par: Boolean)(prog: PrelProg): Seq[Char] => LazyList[Char] = {
    @tailrec
    def prun(i: Int, chr: Chorus, inp: Seq[Char]): Option[(String, (Int, Chorus, Seq[Char]))] = prog.get(i) match{
      case None => None
      case Some(ops) => chr.exec(ops, inp) match{
        case (nchr, out, read, jmp) =>
          val ni = if(jmp) prog.jump(i) else i + 1
          val ninp = if(read) inp.tail else inp
          if(out.isEmpty) prun(ni, nchr, ninp)
          else Some((out.mkString, (ni, nchr, ninp)))}}
          
    val initChorus: Chorus = {
      if(par) SeqChorus(Vector.range(0, prog.numVoices).map(i => Voice(i, LazyList.continually(SafeLong(0)))))
      else ParChorus(ParVector.range(0, prog.numVoices).map(i => Voice(i, LazyList.continually(SafeLong(0)))))}
    inputs => LazyList.unfold((0, initChorus, inputs)){case (i, chr, inp) => prun(i, chr, inp)}.flatten}
  
  def parse(progRaw: String): Try[PrelProg] = {
    val progMat = Matrix.fromString(progRaw)
    @tailrec
    def sdo(i: Int, stk: Vector[Int], ac: immutable.HashMap[Int, Int]): immutable.HashMap[Int, Int] = progMat.colOption(i) match{
      case None => ac
      case Some(ops) =>
        if(ops.contains('(')) sdo(i + 1, i +: stk, ac)
        else if(ops.contains(')')) sdo(i + 1, stk.tail, ac ++ Seq((stk.head, i + 1), (i, stk.head + 1)))
        else sdo(i + 1, stk, ac)}
    Success(PrelProg(progMat, sdo(0, Vector(), immutable.HashMap())))}
  
  trait Chorus{
    def exec(ops: Vector[Char], inp: Seq[Char]): (Chorus, Vector[Char], Boolean, Boolean)
  }
  case class SeqChorus(voices: Vector[Voice]) extends Chorus{
    val size: Int = voices.size
    def voice(i: Int): Voice = voices((i%size + size)%size)
    
    def exec(ops: Vector[Char], inp: Seq[Char]): (Chorus, Vector[Char], Boolean, Boolean) = {
      @tailrec
      def edo(ac: Vector[Voice], src: Vector[(Char, Voice)], out: Vector[Char], read: Boolean, jmp: Boolean): (Chorus, Vector[Char], Boolean, Boolean) = src match{
        case (op, v) +: ps => op match{
          case '+' => edo(ac :+ v.add, ps, out, read, jmp)
          case '-' => edo(ac :+ v.subt, ps, out, read, jmp)
          case '#' => edo(ac :+ v.pop._2, ps, out, read, jmp)
          case '(' => edo(ac :+ v, ps, out, read, jmp || !v.chk)
          case ')' => edo(ac :+ v, ps, out, read, jmp || v.chk)
          case '^' => edo(ac :+ v.push(voice(v.id - 1).head), ps, out, read, jmp)
          case 'v' => edo(ac :+ v.push(voice(v.id + 1).head), ps, out, read, jmp)
          case '?' => edo(ac :+ v.push(SafeLong(inp.head)), ps, out, read=true, jmp)
          case '!' => v.pop match{
            case (n, nv) => edo(ac :+ nv, ps, out :+ n.toChar, read, jmp)}
          case n if n.isDigit => edo(ac :+ v.push(SafeLong(n.asDigit)), ps, out, read, jmp)
          case _ => edo(ac :+ v, ps, out, read, jmp)}
        case _ => (SeqChorus(ac), out, read, jmp)}
      edo(Vector(), ops.zip(voices), Vector(), read=false, jmp=false)}
  }
  case class ParChorus(voices: ParVector[Voice]) extends Chorus{
    lazy val size: Int = voices.size
    def voice(i: Int): Voice = {
      val fid = (i%size + size)%size
      voices.find(_.id == fid).get}
    def exec(ops: Vector[Char], inp: Seq[Char]): (Chorus, Vector[Char], Boolean, Boolean) = {
      def edo(op: Char, v: Voice): (Voice, Boolean, Option[Char]) = op match{
        case '+' => (v.add, false, None)
        case '-' => (v.subt, false, None)
        case '#' => (v.pop._2, false, None)
        case '(' => (v, !v.chk, None)
        case ')' => (v, v.chk, None)
        case '^' => (v.push(voice(v.id - 1).head), false, None)
        case 'v' => (v.push(voice(v.id + 1).head), false, None)
        case '?' => (v.push(SafeLong(inp.head)), false, None)
        case '!' => v.pop match{
          case (n, nv) => (nv, false, Some(n.toChar))}
        case n if n.isDigit => (v.push(SafeLong(n.asDigit)), false, None)
        case _ => (v, false, None)}
      val res = voices.to(ParVector).map(v => edo(ops(v.id), v))
      (ParChorus(res.map(_._1)), res.collect{case (_, _, Some(c)) => c}.toVector, ops.contains('?'), res.exists(_._2))}
  }
  
  case class Voice(id: Int, stk: LazyList[SafeLong]){
    def push(n: SafeLong): Voice = Voice(id, n #:: stk)
    def pop: (SafeLong, Voice) = stk match{
      case n +: ns => (n, Voice(id, ns))}
    def add: Voice = stk match{
      case a +: b +: ns => Voice(id, (a + b) #:: ns)}
    def subt: Voice = stk match{
      case a +: b +: ns => Voice(id, (b - a) #:: ns)}
    def chk: Boolean = stk.head != 0
    def head: SafeLong = stk.head
  }
  
  case class PrelProg(mat: Matrix[Char], jump: immutable.HashMap[Int, Int]){
    val numVoices: Int = mat.xdim
    def apply(i: Int): Vector[Char] = mat.col(i)
    def get(i: Int): Option[Vector[Char]] = mat.colOption(i)
  }
}
