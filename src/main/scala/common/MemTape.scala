package common

import spire.math.Numeric
import spire.implicits._

import scala.collection.Factory

case class MemTape[T : Numeric](vec: Vector[T], dyn: Boolean, blank: T) {
  def +:(e: T): MemTape[T] = MemTape(e +: vec, dyn, blank)
  def :+(e: T): MemTape[T] = MemTape(vec :+ e, dyn, blank)
  
  def apply(i: Int): T = vec.lift(i) match{
    case Some(e) => e
    case None if dyn => blank}
  
  def drop(n: Int): MemTape[T] = MemTape(vec.drop(n), dyn, blank)
  
  def set(i: Int, e: T): MemTape[T] = {
    if(dyn && !vec.isDefinedAt(i)) MemTape(vec.padTo(i, blank) :+ e, dyn, blank)
    else MemTape(vec.updated(i, e), dyn, blank)}
  
  def inc(i: Int, e: T): MemTape[T] = vec.lift(i) match{
    case Some(n) => MemTape(vec.updated(i, n + e), dyn, blank)
    case None if dyn => MemTape(vec.padTo(i, blank) :+ e, dyn, blank)}
  
  def alter(i: Int, f: T => T): MemTape[T] = vec.lift(i) match{
    case Some(e) => MemTape(vec.updated(i, f(e)), dyn, blank)
    case None => MemTape(vec.padTo(i, blank) :+ f(blank), dyn, blank)}
  
  def to[C](factory: Factory[T, C]): C = vec.to(factory)
}