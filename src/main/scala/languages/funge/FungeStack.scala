package languages.funge

case class FungeStack(vec: Vector[Int]){
  def apply(i: Int): Int = vec.lift(i).getOrElse(0)
  
  def take(n: Int): Vector[Int] = vec.take(n).padTo(n, 0)
  def drop(n: Int): FungeStack = FungeStack(vec.drop(n))
  
  def head: Int = vec.headOption.getOrElse(0)
  def tail: FungeStack = FungeStack(vec.drop(1))
  
  def :++(seq: Seq[Int]): FungeStack = FungeStack(vec :++ seq)
  def ++:(seq: Seq[Int]): FungeStack = FungeStack(seq ++: vec)
  
  def +:(n: Int): FungeStack = FungeStack(n +: vec)
  def :+(n: Int): FungeStack = FungeStack(vec :+ n)
  
  def size: Int = vec.size
}
object #-:{
  def unapply(stk: FungeStack): Option[(Int, FungeStack)] = Some((stk.head, stk.tail))
}