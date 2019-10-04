package funge

import java.util.Calendar

import common.{Matrix, Vec2D}

import scala.annotation.tailrec

case class BF98Prog(prog: Matrix[Int], origin: Vec2D[Int], cal: Calendar, bDiv: Boolean){
  def apply(p: Vec2D[Int]): Int = {
    val pnt = p + origin
    if(prog.isDefinedAt(pnt.x, pnt.y)) prog(pnt.x, pnt.y)
    else 32
  }
  def get(p: Vec2D[Int]): Option[Int] = {
    val pnt = p + origin
    prog.get(pnt.x, pnt.y)
  }
  
  def getNextInd(ip: Vec2D[Int], dt: Vec2D[Int]): Vec2D[Int] = {
    @tailrec
    def scrub(pos: Vec2D[Int]): Vec2D[Int] = {
      get(pos) match{
        case Some(32) => scrub(pos + dt)
        case Some(59) => scrub(skipTill(pos + dt, dt, 59) + dt)
        case Some(_) => pos
        case None => scrub(wrap(pos, dt))
      }
    }
    scrub(ip + dt)
  }
  def getNext(ip: Vec2D[Int], dt: Vec2D[Int]): (Vec2D[Int], Int) = {
    val npos = getNextInd(ip, dt)
    (npos, this.apply(npos))
  }
  
  def updated(p: Vec2D[Int], e: Int): BF98Prog = {
    val (pprog, norg) = padToMat(p)
    val pos = norg + p
    val nprog = pprog.updated(pos.x, pos.y, e)
    BF98Prog(nprog, norg, cal, bDiv)
  }
  
  def skipTill(pos: Vec2D[Int], dt: Vec2D[Int], e: Int): Vec2D[Int] = {
    @tailrec def sdo(p: Vec2D[Int]): Vec2D[Int] = {
      get(p) match{
        case Some(`e`) => p
        case Some(_) => sdo(p + dt)
        case None => sdo(wrap(p, dt))
      }
    }
    sdo(pos)
  }
  def skipAll(pos: Vec2D[Int], dt: Vec2D[Int], e: Int): Vec2D[Int] = {
    @tailrec def sdo(p: Vec2D[Int]): Vec2D[Int] = if(this.apply(p) == e) sdo(p + dt) else p
    sdo(pos)
  }
  def skipN(pos: Vec2D[Int], dt: Vec2D[Int], num: Int): Vec2D[Int] = {
    val sdt = dt*num.sign
    
    @tailrec def sdo(p: Vec2D[Int], n: Int): Vec2D[Int] = {
      if(n == 0) p
      else sdo(getNextInd(p, sdt), n - 1)
    }
    
    sdo(pos, num.abs)
  }
  
  def wrap(ip: Vec2D[Int], dt: Vec2D[Int]): Vec2D[Int] = {
    val pos = origin + ip
    if(prog.isDefinedAt(pos.x, pos.y)) ip
    else{
      val rev = -dt
      val s: Int = {
        lazy val xs = if(rev.x < 0) math.floor(pos.x/rev.x.abs).toInt else math.floor((prog.xdim - 1 - pos.x)/rev.x).toInt
        lazy val ys = if(rev.y < 0) math.floor(pos.y/rev.y.abs).toInt else math.floor((prog.ydim - 1 - pos.y)/rev.y).toInt
        
        if(rev.x != 0 && rev.y != 0) math.min(xs, ys)
        else if(rev.x != 0) xs
        else if(rev.y != 0) ys
        else 0
      }
      ip - dt*s
    }
  }
  
  def padToMat(p: Vec2D[Int]): (Matrix[Int], Vec2D[Int]) = {
    val pnt = origin + p
    
    val x0 = if(pnt.x < 0) pnt.x.abs else 0
    val x1 = (pnt.x - prog.xdim + 1).max(0)
    val y0 = if(pnt.y < 0) pnt.y.abs else 0
    val y1 = (pnt.y - prog.ydim + 1).max(0)
    
    val norg = origin + Vec2D[Int](x0, y0)
    val nprog = prog.padWith(x0, x1, y0, y1, 32)
    
    (nprog, norg)
  }
  def padTo(p: Vec2D[Int]): BF98Prog = {
    val (nprog, norg) = padToMat(p)
    BF98Prog(nprog, norg, cal, bDiv)
  }
  
  def getBounds: (Vec2D[Int], Vec2D[Int]) = {
    (-origin, origin + Vec2D(prog.xdim, prog.ydim))
  }
}
object BF98Prog{
  def apply(progRaw: String, cal: Calendar, bDiv: Boolean): BF98Prog = {
    val lines = progRaw.linesIterator.map(_.toVector.map(_.toInt)).toVector
    val xdim = lines.map(_.size).max
    val padded = lines.map(v => v.padTo(xdim, 32))
    new BF98Prog(Matrix(padded), Vec2D(0, 0), cal, bDiv)
  }
}
