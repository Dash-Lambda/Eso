package common

case class Matrix[T](vec: Vector[T], xdim: Int, ydim: Int){
  def apply(x: Int, y: Int): T = {
    require(isDefinedAt(x, y), "Index Out Of Bounds")
    vec(getInd(x, y))
  }
  def get(x: Int, y: Int): Option[T] = {
    if(isDefinedAt(x, y)) Some(vec(getInd(x, y)))
    else None
  }
  
  def updated(x: Int, y: Int, e: T): Matrix[T] = {
    require(isDefinedAt(x, y), "Index Out Of Bounds")
    Matrix[T](vec.updated(getInd(x, y), e), xdim, ydim)
  }
  
  def transpose: Matrix[T] = {
    val trans = vec
      .grouped(xdim)
      .toVector
      .transpose
      .flatten
    Matrix[T](trans, ydim, xdim)
  }
  
  def padWith(x0: Int, x1: Int, y0: Int, y1: Int, e: T): Matrix[T] = {
    val padder = Vector.fill(x0)(e)
    val nvec = vec
      .grouped(xdim)
      .flatMap(v => padder ++ v.padTo(xdim + x1, e))
      .toVector
    val pvec = Vector.fill((x0 + xdim + x1)*y0)(e) ++ nvec ++ Vector.fill((x0 + xdim + x1)*y1)(e)
    Matrix[T](pvec, x0 + xdim + x1, y0 + ydim + y1)
  }
  def padTo(x: Int, y: Int, e: T): Matrix[T] = {
    val x0 = if(x < 0) x.abs else 0
    val x1 = if(x >= xdim) x + 1 - xdim else 0
    val y0 = if(y < 0) y.abs else 0
    val y1 = if(y >= ydim) y + 1 - ydim else 0
    //println(s"- padTo: <$x, $y> => ($x0, $x1, $y0, $y1)")
    padWith(x0, y0, x1, y1, e)
  }
  
  def isDefinedAt(x: Int, y: Int): Boolean = 0 <= x && x < xdim && 0 <= y && y < ydim
  def getInd(x: Int, y: Int): Int = (y*xdim) + x
}
object Matrix{
  def apply[T](vecs: Vector[Vector[T]]): Matrix[T] = {
    require(vecs.forall(v => v.sizeIs == vecs.head.size), "Matrix Must Be Rectangular")
    val xdim = vecs.head.size
    val ydim = vecs.size
    new Matrix[T](vecs.flatten, xdim, ydim)
  }
  
  def charString(mat: Matrix[Int]): String = mat.vec.map(_.toChar).grouped(mat.xdim).map(_.mkString).mkString("\n")
}