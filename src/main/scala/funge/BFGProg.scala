package funge

case class BFGProg(prog: Vector[Char], xdim: Int, ydim: Int){
  def apply(x: Int, y: Int): Char = prog(getInd(x, y))
  def apply(x: Long, y: Long): Char = prog(getInd(x.toInt, y.toInt))
  def get(x: Int, y: Int): Option[Char] = {
    if(0 <= x && x < xdim && 0 <= y && y < ydim) Some(prog((xdim*y) + x))
    else None
  }
  
  def updated(x: Int, y: Int, c: Char): BFGProg = new BFGProg(prog.updated(getInd(x, y), c), xdim, ydim)
  
  def getInd(x: Int, y: Int): Int = (xdim*math.floorMod(y, ydim)) + math.floorMod(x, xdim)
  override def toString: String = prog.mkString.grouped(xdim).mkString("\n")
}
object BFGProg{
  def apply(progRaw: Seq[Seq[Char]], xdim: Int, ydim: Int): BFGProg = {
    val conditioned = progRaw.padTo(ydim, Seq()).take(ydim).flatMap(_.padTo(xdim, ' ').take(xdim)).toVector
    new BFGProg(conditioned, xdim, ydim)
  }
  def apply(progRaw: String, xdim: Int, ydim: Int): BFGProg = this.apply(progRaw.linesIterator.map(_.toVector).toVector, xdim, ydim)
  def apply(progRaw: String): BFGProg = {
    val lines = progRaw.linesIterator.map(_.toVector).toVector
    val xdim = lines.map(_.length).max
    val ydim = lines.size
    this.apply(lines, xdim, ydim)
  }
}
