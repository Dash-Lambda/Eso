package Compilers

import scala.collection.mutable
import scala.util.Try

trait Compiler {
  def name: (String, String) = (src, dst)
  def src: String
  def dst: String
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String]
  
  def getParms(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(bams: String*)(nams: String*): Option[(Vector[Boolean], Vector[Int])] = {
    if(bams.forall(bools.isDefinedAt) && nams.forall(nums.isDefinedAt)){
      val bals = bams.map(str => bools(str)._1).toVector
      val nals = nams.map(str => nums(str)._1).toVector
      Some((bals, nals))
    }else None
  }
}
