package funge

import common.{EsoObj, Matrix, Vec2D}

import scala.annotation.tailrec

case class BF98Prog(prog: Matrix[Int], origin: Vec2D[Int], bDiv: Boolean) extends EsoObj{
  def apply(p: Vec2D[Int]): Int = prog.getOrElse(p + origin)(32)
  def get(p: Vec2D[Int]): Option[Int] = prog.get(p + origin)
  
  def getNextInd(ip: Vec2D[Int], dt: Vec2D[Int]): Vec2D[Int] = {
    @tailrec
    def scrub(pos: Vec2D[Int]): Vec2D[Int] = get(pos) match{
      case Some(n) => n match{
        case 32 => scrub(pos + dt)
        case 59 => scrub(skipTill(pos + dt, dt, 59) + dt)
        case _ => pos}
      case None => scrub(wrap(pos, dt))}
    scrub(ip + dt)}
  def getNext(ip: Vec2D[Int], dt: Vec2D[Int]): (Vec2D[Int], Int) = {
    val npos = getNextInd(ip, dt)
    (npos, this.apply(npos))}
  
  def updated(p: Vec2D[Int], e: Int): BF98Prog = padToMat(p) match{
    case (pprog, norg) => BF98Prog(pprog.updated(norg + p, e), norg, bDiv)}
  
  def skipTill(pos: Vec2D[Int], dt: Vec2D[Int], e: Int): Vec2D[Int] = {
    @tailrec def sdo(p: Vec2D[Int]): Vec2D[Int] = get(p) match{
      case Some(c) => c match{
        case `e` => p
        case _ => sdo(p + dt)}
      case None => sdo(wrap(p, dt))}
    sdo(pos)}
  
  def skipAll(pos: Vec2D[Int], dt: Vec2D[Int], e: Int): Vec2D[Int] = {
    @tailrec
    def sdo(p: Vec2D[Int]): Vec2D[Int] = get(p) match{
      case Some(c) => c match{
        case `e` => sdo(p + dt)
        case _ => p}
      case None => sdo(wrap(p, dt))}
    sdo(pos)}
  
  def skipN(pos: Vec2D[Int], dt: Vec2D[Int], num: Int): Vec2D[Int] = {
    val sdt = dt*num.sign
    @tailrec
    def sdo(p: Vec2D[Int], n: Int): Vec2D[Int] = n match{
      case 0 => p
      case _ => sdo(getNextInd(p, sdt), n - 1)}
    sdo(pos, num.abs)}
  
  def wrap(ip: Vec2D[Int], dt: Vec2D[Int]): Vec2D[Int] = {
    val pos = origin + ip
    if(prog.isDefinedAt(pos)) ip
    else{
      val rev = -dt
      val s: Int = {
        lazy val xs = if(rev.x < 0) math.floor(pos.x/rev.x.abs).toInt else math.floor((prog.xdim - 1 - pos.x)/rev.x).toInt
        lazy val ys = if(rev.y < 0) math.floor(pos.y/rev.y.abs).toInt else math.floor((prog.ydim - 1 - pos.y)/rev.y).toInt
        
        if(rev.x != 0 && rev.y != 0) math.min(xs, ys)
        else if(rev.x != 0) xs
        else if(rev.y != 0) ys
        else 0}
      ip - dt*s}}
  
  def padToMat(p: Vec2D[Int]): (Matrix[Int], Vec2D[Int]) = {
    val pnt = origin + p
    val x0 = if(pnt.x < 0) pnt.x.abs else 0
    val x1 = (pnt.x - prog.xdim + 1).max(0)
    val y0 = if(pnt.y < 0) pnt.y.abs else 0
    val y1 = (pnt.y - prog.ydim + 1).max(0)
    
    (prog.padWith(x0, x1, y0, y1, 32), origin + Vec2D[Int](x0, y0))}
  def padTo(p: Vec2D[Int]): BF98Prog = padToMat(p) match{
    case (nprog, norg) => BF98Prog(nprog, norg, bDiv)}
  
  def getBounds: (Vec2D[Int], Vec2D[Int]) = {
    val vecs = prog.vec.grouped(prog.xdim).toVector
    val tran = vecs.transpose
    
    val x0 = tran.indexWhere(_.exists(_ != 32))
    val x1 = tran.lastIndexWhere(_.exists(_ != 32))
    val y0 = vecs.indexWhere(_.exists(_ != 32))
    val y1 = vecs.lastIndexWhere(_.exists(_ != 32))
    
    val least = Vec2D(x0, y0)
    
    (least - origin, Vec2D(x1, y1) - least)}
}
object BF98Prog extends EsoObj{
  def apply(progRaw: String, bDiv: Boolean): BF98Prog = new BF98Prog(Matrix(StringToRect(progRaw)), Vec2D(0, 0), bDiv)
}
