package wierd

import common.{EsoObj, Vec2D}

import scala.annotation.tailrec
import scala.util.Random

trait WIPRet
case class WIPCont(wip: WIP, inp: Seq[Char], prog: WierdProg) extends WIPRet
case class WIPOut(out: Char, wip: WIP) extends WIPRet
case class WIPSplit(wip1: WIP, wip2: WIP) extends WIPRet
object WIPHalt extends WIPRet

case class WIP(ip: Vec2D[Int], dt: Vec2D[Int], stk: LazyList[Int]){
  def apply(inp: Seq[Char], prog: WierdProg): WIPRet = {
    if(prog.isLine(ip + dt)) WIPCont(WIP(ip + dt, dt, stk), inp, prog)
    else tryTurn(ip, dt, prog) match{
      case ((ln, lv), (rn, rv)) if ln == rn => ln match{
        case 0 => WIPCont(WIP(ip + dt, dt, stk), inp, prog)
        case 1 =>
          val ndt = if(prog.rand.nextInt(2) == 0) lv else rv
          WIPCont(WIP(ip + ndt, ndt, stk), inp, prog)
        case 2 => WIPSplit(WIP(ip + rv, rv, stk), WIP(ip + lv, lv, stk))
        case 3 => WIPHalt
        case 4 => WIPHalt}
      case ((ln, lv), (rn, _)) if ln < rn => ln match{
        case 0 => WIPCont(WIP(ip + dt, dt, stk), inp, prog)
        case 1 => WIPCont(WIP(ip + lv, lv, 1 +: stk), inp, prog)
        case 2 =>
          if(stk.head == 0) WIPCont(WIP(ip + lv, lv, stk.tail), inp, prog)
          else WIPCont(WIP(ip - dt, -dt, stk.tail), inp, prog)
        case 3 => stk match{
          case c +: y +: x +: ns =>
            if(c != 0) WIPCont(WIP(ip + lv, lv, prog(x, y) #:: ns), inp, prog)
            else ns match{
              case e +: es => WIPCont(WIP(ip + lv, lv, es), inp, prog.updated(Vec2D(x, y), e))}}
        case 4 =>
          if(prog.isLine(ip + (dt*2))) WIPCont(WIP(ip + (dt*2), dt, stk), inp, prog)
          else WIPHalt}
      case ((ln, _), (rn, rv)) if ln > rn => rn match{
        case 0 => WIPCont(WIP(ip + dt, dt, stk), inp, prog)
        case 1 => stk match{
          case b +: a +: ns => WIPCont(WIP(ip + rv, rv, (a - b) #:: ns), inp, prog)}
        case 2 =>
          if(stk.head == 0) WIPCont(WIP(ip + rv, rv, stk.tail), inp, prog)
          else WIPCont(WIP(ip - dt, dt, stk.tail), inp, prog)
        case 3 => stk match{
          case c +: ns =>
            if(c == 0) WIPCont(WIP(ip + rv, rv, inp.head.toInt +: ns), inp.tail, prog)
            else ns match {
              case e +: es => WIPOut(e.toChar, WIP(ip + rv, rv, es))}}
        case 4 => WIPCont(WIP(ip + dt, dt, stk), inp, prog)}}}
  
  @tailrec
  private def turn(v: Vec2D[Int], n: Int): Vec2D[Int] = {
    if(n > 0) turn(Vec2D((v.x + v.y).sign, (v.y - v.x).sign), n - 1)
    else if(n < 0) turn(Vec2D((v.x - v.y).sign, (v.y + v.x).sign), n + 1)
    else v}
  def tryTurn(p: Vec2D[Int], d: Vec2D[Int], prog: WierdProg): ((Int, Vec2D[Int]), (Int, Vec2D[Int])) = {
    val nums = LazyList.range(0, 5)
    val ls = nums map (n => (n, turn(d, n))) filter {case (_, v) => prog.isLine(p + v)}
    val rs = nums map (n => (n, turn(d, -n))) filter {case (_, v) => prog.isLine(p + v)}
    (ls.head, rs.head)}
}

case class WierdProg(vecs: Vector[Vector[Int]], origin: Vec2D[Int], rand: Random) extends EsoObj{
  def apply(p: Vec2D[Int]): Int = if(isDefinedAt(origin + p)) vecs(origin.y + p.y)(origin.x + p.x) else 32
  def apply(x: Int, y: Int): Int = apply(Vec2D(x, y))
  
  def updated(p: Vec2D[Int], e: Int): WierdProg = {
    val v = origin + p
    val padded = padTo(vecs, v)
    val nvecs = padded.updated(v.y, padded(v.y).updated(v.x, e))
    val nx = if(v.x < 0) origin.x - v.x else origin.x
    val ny = if(v.y < 0) origin.y - v.y else origin.y
    WierdProg(nvecs, Vec2D(nx, ny), rand)}
  
  def padTo(vs: Vector[Vector[Int]], p: Vec2D[Int]): Vector[Vector[Int]] = {
    val pady = vs.padTo(p.y + 1, Vector())
    val padx = pady.updated(p.y, pady(p.y).padTo(p.x + 1, 32))
    padx}
  
  def isLine(p: Vec2D[Int]): Boolean = !apply(p).toChar.isWhitespace
  def isDefinedAt(p: Vec2D[Int]): Boolean = p.x >= 0 && p.y >= 0 && p.y < vecs.size && p.x < vecs(p.y).size
}
object WierdProg{
  def apply(str: String, rand: Random): WierdProg = {
    val vecs = str.linesIterator.map(_.toVector.map(_.toInt)).toVector
    new WierdProg(vecs, Vec2D(-1, -1), rand)}
}