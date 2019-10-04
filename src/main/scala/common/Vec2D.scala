package common

import spire.math.Numeric
import spire.implicits._

case class Vec2D[N: Numeric](x: N, y: N){
  def +(v: Vec2D[N]): Vec2D[N] = Vec2D(x + v.x, y + v.y)
  def -(v: Vec2D[N]): Vec2D[N] = Vec2D(x - v.x, y - v.y)
  def *(v: Vec2D[N]): Vec2D[N] = Vec2D(x*v.x, y*v.y)
  def *(c: N): Vec2D[N] = Vec2D(c*x, c*y)
  def /(v: Vec2D[N]): Vec2D[N] = Vec2D(x/v.x, y/v.y)
  def unary_- : Vec2D[N] = Vec2D(-x, -y)
  
  def abs: Vec2D[N] = Vec2D[N](x.abs, y.abs)
  def mag: N = ((x*x) + (y*y)).sqrt
  
  override def toString: String = s"<$x,$y>"
}
object Vec2D{
  def apply[N: Numeric](x: N, y: N): Vec2D[N] = new Vec2D[N](x, y)
}