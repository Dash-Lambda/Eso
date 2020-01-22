package common

import scala.collection.{immutable, mutable}
import scala.util.{Random, Success, Try}

case class Config(bool: immutable.HashMap[String, Boolean], num: immutable.HashMap[String, Int], times: LazyList[Long], rands: LazyList[Int]) extends EsoObj{
  def apply(tag: String): Either[Int, Boolean] = Either.cond(bool.isDefinedAt(tag), bool(tag), num(tag))
  def get(tag: String): Option[Either[Int, Boolean]] = Try{apply(tag)} match{
    case Success(res) => Some(res)
    case _ => None}
  
  def bools: Vector[(String, Boolean)] = bool.toVector.sortBy(_._1)
  def nums: Vector[(String, Int)] = num.toVector.sortBy(_._1)
  
  def randInt: (Int, Config) = rands match{
    case r +: rs => (r, Config(bool, num, times, rs))}
  
  def set(nam: String, b: Boolean): Config = Config(bool + ((nam, b)), num, times, rands)
  def set(nam: String, n: Int): Config = Config(bool, num + ((nam, n)), times, rands)
}
object Config extends EsoObj{
  def blank: Config = {
    val rand = new Random()
    Config(immutable.HashMap(), immutable.HashMap(), LazyList(), LazyList.continually(rand.nextInt))}
  def apply(bool: mutable.HashMap[String, Boolean], num: mutable.HashMap[String, Int]): Config = {
    val rand = new Random()
    Config(mkImmut(bool), mkImmut(num), LazyList.continually(System.currentTimeMillis), LazyList.continually(rand.nextInt))}
  def apply(bool: immutable.HashMap[String, Boolean], num: immutable.HashMap[String, Int]): Config = {
    val rand = new Random()
    Config(bool, num, LazyList.continually(System.currentTimeMillis), LazyList.continually(rand.nextInt))}
}