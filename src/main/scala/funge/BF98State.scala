package funge

case class BF98State(times: LazyList[Long], inp: Seq[Char], rands: LazyList[Int], fpDat: BF98FPData){
  def readTime: (Long, BF98State) = times match{
    case t +: ts => (t, BF98State(ts, inp, rands, fpDat))}
  def popTime: BF98State = BF98State(times.tail, inp, rands, fpDat)
  
  def readInt: (Int, BF98State) = inp match{
    case c +: cs => (c.toInt, BF98State(times, cs, rands, fpDat))}
  def readNum: (Int, BF98State) = (inp.takeWhile(_.isDigit).mkString.toInt, BF98State(times, inp.dropWhile(c => !c.isDigit).dropWhile(_.isDigit), rands, fpDat))
  
  def randInt: (Int, BF98State) = rands match{
    case r +: rs => (r, BF98State(times, inp, rs, fpDat))}
}
