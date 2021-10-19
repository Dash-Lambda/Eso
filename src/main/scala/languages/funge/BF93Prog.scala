package languages.funge

case class BF93Prog(prog: Vector[Char]){
  def apply(x: Int, y: Int): Char = prog(getInd(x, y))
  def apply(x: Long, y: Long): Char = prog(getInd(x.toInt, y.toInt))
  def get(x: Int, y: Int): Option[Char] = {
    if(0 <= x && x < 80 && 0 <= y && y < 25) Some(prog((80*y) + x))
    else None}
  
  def updated(x: Int, y: Int, c: Char): BF93Prog = new BF93Prog(prog.updated(getInd(x, y), c))
  
  def getInd(x: Int, y: Int): Int = (80*math.floorMod(y, 25)) + math.floorMod(x, 80)
  override def toString: String = prog.mkString.grouped(80).mkString("\n")
}
object BF93Prog{
  def apply(progRaw: String): BF93Prog = this.apply(progRaw.linesIterator.map(_.toVector).toVector)
  def apply(progRaw: Seq[Seq[Char]]): BF93Prog = BF93Prog(progRaw.padTo(25, Seq()).take(25).flatMap(_.padTo(80, ' ').take(80)).toVector)
}
