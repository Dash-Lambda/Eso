package funge

import scala.util.Random

case class BF98State(times: LazyList[Long], inp: Seq[Char], rand: Random, fpDat: BF98FPData){
  def readTime: (Long, BF98State) = times match{
    case t +: ts => (t, BF98State(ts, inp, rand, fpDat))}
  
  def readInt: (Int, BF98State) = inp match{
    case c +: cs => (c.toInt, BF98State(times, cs, rand, fpDat))}
  def readNum: (Int, BF98State) = (inp.takeWhile(_.isDigit).mkString.toInt, BF98State(times, inp.dropWhile(c => !c.isDigit).dropWhile(_.isDigit), rand, fpDat))
}
