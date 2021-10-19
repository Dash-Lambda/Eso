package languages.funge

import scala.collection.immutable

case class BF98FPData(hrtiMarks: immutable.HashMap[Int, Long], vecRefs: Vector[(Int, Int)]){
  def markTime(id: Int, t: Long): BF98FPData = BF98FPData(hrtiMarks + ((id, t)), vecRefs)
  def unMarkTime(id: Int): BF98FPData = BF98FPData(hrtiMarks - id, vecRefs)
  def pushRef(y: Int, x: Int): (Int, BF98FPData) = (vecRefs.size, BF98FPData(hrtiMarks, vecRefs :+ (x, y)))
}
object BF98FPData{
  def default: BF98FPData = BF98FPData(immutable.HashMap(), Vector())
}