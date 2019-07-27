package common

import scala.collection.{immutable, mutable}

trait EsoObj {
  def getParms(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(bams: String*)(nams: String*): Option[(Vector[Boolean], Vector[Int])] = {
    if(bams.forall(bools.isDefinedAt) && nams.forall(nums.isDefinedAt)){
      val bals = bams.map(str => bools(str)._1).toVector
      val nals = nams.map(str => nums(str)._1).toVector
      Some((bals, nals))
    }else None
  }
  
  def percBar(p: Int, t: Int): String = {
    val per = 100 - (p * 100) / t
    val dots = "." * ((per - 1) / 10)
    s"\u001B[100D$per% $dots"
  }
  
  def buildMap(ks: Vector[String], vs: Vector[String]): immutable.HashMap[String, String] = mkMap(ks.zip(vs))
  def mkMap[A, B](vec: Vector[(A, B)]): immutable.HashMap[A, B] = {
    val builder = immutable.HashMap.newBuilder[A, B]
    builder ++= vec
    builder.result
  }
}
