package common

object TimeIt extends EsoObj{
  def apply[A](f: => A): (A, Long) = {
    val t = System.currentTimeMillis
    val res = f
    (res, System.currentTimeMillis - t)}
  
  def time[A](f: => A): Long = {
    val t = System.currentTimeMillis
    f
    System.currentTimeMillis - t}
}
